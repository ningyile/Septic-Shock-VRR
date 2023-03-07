from numpy.core.fromnumeric import argmax
import pandas as pd
import numpy as np
from tqdm.auto import tqdm

import torch
import torch.autograd as autograd
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader

import csv

import pytorch_lightning as pl
import seaborn as sns
from pylab import rcParams
import matplotlib.pyplot as plt
from matplotlib import rc
from matplotlib.ticker import MaxNLocator

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder

from multiprocessing import cpu_count
from pytorch_lightning.callbacks import ModelCheckpoint, EarlyStopping
from pytorch_lightning.loggers import TensorBoardLogger
from pytorch_lightning.metrics.functional import accuracy
from sklearn.metrics import classification_report, confusion_matrix



class VIS_Dataset(Dataset):
    def __init__(self, sequences):
        self.sequences = sequences
        
    def __len__(self):
        return len(self.sequences)
    
    def __getitem__(self, idx):
        sequence, label = self.sequences[idx]
        return dict(
            sequence=torch.Tensor(sequence.to_numpy()),
            label=torch.tensor(label).long()
        )

class VIS_DataModule(pl.LightningDataModule):
    
    def __init__(self, train_sequences, test_sequences, batch_size):
        super().__init__()
        self.train_sequences = train_sequences
        self.test_sequences = test_sequences
        self.batch_size = batch_size
        
    def setup(self, stage=None):
        self.train_dataset = VIS_Dataset(self.train_sequences)
        self.test_dataset = VIS_Dataset(self.test_sequences)
    
    def train_dataloader(self):
        return DataLoader(
            self.train_dataset,
            batch_size=self.batch_size,
            shuffle=True,
            num_workers=0
        )
    
        
    def val_dataloader(self):
        return DataLoader(
            self.test_dataset,
            batch_size=self.batch_size,
            shuffle=False,
            num_workers=0
        )
    
    def test_dataloader(self):
        return DataLoader(
            self.test_dataset,
            batch_size=self.batch_size,
            shuffle=False,
            num_workers=0
        )



class SequenceModel(nn.Module):
    def __init__(self, n_features, n_classes, n_hidden=128, n_layers=3):
        super().__init__()
        
        self.lstm = nn.LSTM(
            input_size=n_features,
            hidden_size=n_hidden,
            num_layers=n_layers,
            batch_first=True,
            dropout=0.25
        )
    
        self.classifier = nn.Linear(n_hidden, n_classes)

    def forward(self, x):
        self.lstm.flatten_parameters()
        _, (hidden, _) = self.lstm(x)
        
        out = hidden[-1]
        return self.classifier(out)

class VIS_Predictor(pl.LightningModule):

    def __init__(self, n_features: int, n_classes: int):
        super().__init__()
        self.model = SequenceModel(n_features, n_classes)
        self.criterion = nn.CrossEntropyLoss()
        
        
    def forward(self , x, labels=None):
        output = self.model(x)
        loss = 0
        if labels is not None :
            loss = self.criterion(output, labels)
        return loss, output

    def training_step(self, batch, batch_idx):
        sequences = batch["sequence"]
        labels = batch["label"]
        loss, outputs = self(sequences, labels)
        predictions = torch.argmax (outputs, dim=1)
        step_accuracy = accuracy(predictions, labels)
        
        self.log("train_loss", loss, prog_bar=True, logger=True)
        self.log("train_accuracy", step_accuracy, prog_bar=True, logger=True)
        return {"loss": loss, "accuracy": step_accuracy}
    
    def validation_step(self, batch, batch_idx):
        sequences = batch["sequence"]
        labels = batch["label"]
        loss, outputs = self(sequences, labels)
        predictions = torch.argmax (outputs, dim=1)
        step_accuracy = accuracy(predictions, labels)
        
        self.log("val_loss", loss, prog_bar=True, logger=True)
        self.log("val_accuracy", step_accuracy, prog_bar=True, logger=True)
        return {"loss": loss, "accuracy": step_accuracy}
    
    def test_step(self, batch, batch_idx):
        sequences = batch["sequence"]
        labels = batch["label"]
        loss, outputs = self(sequences, labels)
        predictions = torch.argmax (outputs, dim=1)
        step_accuracy = accuracy(predictions, labels)
        
        self.log("test_loss", loss, prog_bar=True, logger=True)
        self.log("test_accuracy", step_accuracy, prog_bar=True, logger=True)
        return {"loss": loss, "accuracy": step_accuracy}
    
    def configure_optimizers(self):
        return optim.Adam(self.parameters(), lr=0.0001)
        # return optim.Adam(self.parameters(), lr=0.0001)



pl.seed_everything(100)

# N_EPOCHS = 220
# BATCH_SIZE = 16
N_EPOCHS = 200
BATCH_SIZE = 64


def data_to_sequence(x_train, y_train):
    label_encoder = LabelEncoder()
    encoded_labels = label_encoder.fit_transform(y_train.hos_outcome)
    y_train["label"] = encoded_labels

    FEATURE_COLUMNS = x_train.columns.tolist()[3:]

    sequences = []

    for stay_id, group in x_train.groupby("stay_id"):
        sequence_features = group[FEATURE_COLUMNS]
        label = y_train[y_train.stay_id == stay_id].iloc[0].label
        
        sequences.append((sequence_features,label))

    return sequences




def train_run(x_train = [], y_train = [], x_test=[], y_test=[]):

    train_sequences = data_to_sequence(x_train=x_train, y_train=y_train)
    test_sequences = data_to_sequence(x_train=x_test, y_train=y_test)
    
    FEATURE_COLUMNS = x_train.columns.tolist()[3:]
    label_encoder = LabelEncoder()
    encoded_labels = label_encoder.fit_transform(y_train.hos_outcome)
    
    # train_sequences, test_sequences = train_test_split(sequences, test_size=0.2)
    data_module = VIS_DataModule(train_sequences, test_sequences, BATCH_SIZE)
    model = VIS_Predictor( 
        n_features=len(FEATURE_COLUMNS),
        n_classes=len(label_encoder.classes_)
    )
    
    checkpoint_callback = ModelCheckpoint(
        dirpath="checkpoints",
        filename= "best-checkpoint",
        save_top_k=1,
        verbose=True,
        monitor="val_loss",
        mode="min"
    )
    
    logger = TensorBoardLogger("lightning_logs", name="hos_outcome")
    
    trainer = pl.Trainer(
        logger=logger,
        checkpoint_callback=checkpoint_callback,
        max_epochs=N_EPOCHS,
        gpus=1,
        progress_bar_refresh_rate=30
    )
    
    trainer.fit(model, data_module)
    
    test_accuracy = trainer.test()[0]['test_accuracy']

    trained_model = VIS_Predictor.load_from_checkpoint(
        trainer.checkpoint_callback.best_model_path,
        n_features=len(FEATURE_COLUMNS),
        n_classes=len(label_encoder.classes_)
    )
        
    trained_model.freeze()

    test_dataset = VIS_Dataset(test_sequences)
    
    predictions = []
    labels = []
    probs_of_pre = []
    outcome_class_of_pre = []

    for item in tqdm (test_dataset):          
        sequence = item["sequence"]
        label = item["label"]
    
        _, output = trained_model(sequence.unsqueeze(dim=0))
        prediction = torch.argmax(output, dim=1)
        predictions.append(prediction.item())
        labels.append(label.item())
        prob = torch.softmax(output, dim=1)
        top_p, top_class = prob.topk(1, dim = 1)
        probs_of_pre.append(top_p)
        outcome_class_of_pre.append(top_class)

    return test_accuracy, labels, predictions, probs_of_pre, outcome_class_of_pre



def write_predictions(cnt, labels, predictions, probs_of_pre, outcome_class_of_pre):
    f = open("./02-vis_overall/csv/vis_lstm_prediction/hos_outcome/prediction_%dh.csv" % cnt, "w", newline="")
    writer = csv.writer(f)
    writer.writerow(["labels", "predictions", "probs_of_pre_raw", "outcome_class_of_pre_raw", "probs_of_pre", "outcome_class_of_pre"])
    for i in range(len(labels)):    
        outcome_class_of_pre_val = outcome_class_of_pre[i].item()
        probs_of_pre_val = probs_of_pre[i].item()
        if outcome_class_of_pre_val == 0 :
            probs_of_pre_val = 1 - probs_of_pre_val
        writer.writerow([labels[i], predictions[i], probs_of_pre[i].item(), outcome_class_of_pre[i].item(), probs_of_pre_val, 1])
    f.close()



x_train = []
y_train = []
result = []

for x_index in range(1, 49):
    x_train = pd.read_csv("./02-vis_overall/csv/vis_lstm_matrix/hos_outcome/x_train_%dh.csv" % x_index)
    y_train = pd.read_csv("./02-vis_overall/csv/vis_lstm_matrix/hos_outcome/y_train_%dh.csv" % x_index)
    x_test = pd.read_csv("./02-vis_overall/csv/vis_lstm_matrix/hos_outcome/x_test_%dh.csv" % x_index)
    y_test = pd.read_csv("./02-vis_overall/csv/vis_lstm_matrix/hos_outcome/y_test_%dh.csv" % x_index)

    test_accuracy, labels, predictions, probs_of_pre, outcome_class_of_pre = train_run(x_train, y_train, x_test, y_test)

    length = x_train.stay_id.value_counts().values[0]
    result.append([length, test_accuracy])

    write_predictions(x_index, labels, predictions, probs_of_pre, outcome_class_of_pre)

print(result)

f = open('./02-vis_overall/csv/vis_lstm_prediction/hos_outcome/accuracy.csv','w', newline="")
writer = csv.writer(f)
for row in result:
    writer.writerow(row)
f.close()
