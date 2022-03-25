# -*- coding: utf-8 -*-
"""
Created on Wed Nov 25 10:02:38 2020

@author: yashr
"""


import torch
import os
import numpy as np
import pandas as pd
from tqdm import tqdm
import seaborn as sns
from pylab import rcParams
import matplotlib.pyplot as plt
from matplotlib import rc
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, classification_report

from torch import nn, optim
import torch.nn.functional as F

sns.set(style='whitegrid', palette='muted', font_scale=1.2)
HAPPY_COLORS_PALETTE =\
["#01BEFE", "#FFDD00", "#FF7D00", "#FF006D", "#93D30C", "#8F00FF"]
sns.set_palette(sns.color_palette(HAPPY_COLORS_PALETTE))
rcParams['figure.figsize'] = 12, 8
RANDOM_SEED = 42
np.random.seed(RANDOM_SEED)
torch.manual_seed(RANDOM_SEED)


df = pd.read_csv('50_Startups.csv')

df.shape
print(df.shape)


cols = ['R&D Spend', 'Administration', 'Marketing Spend', 'State', 'Profit']
df = df[cols]


df = df.dropna(how='any')
print(df)


sns.countplot(df.Profit);

df.Profit.value_counts() / df.shape[0]
print(df.Profit.value_counts() / df.shape[0])

X = df[['R&D Spend', 'Administration', 'Marketing Spend', 'State']]
y = df[['Profit']]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=RANDOM_SEED)


class Net(nn.Module):
  def __init__(self, n_features):
    super(Net, self).__init__()
    self.fc1 = nn.Linear(n_features, 5)
    self.fc2 = nn.Linear(5, 3)
    self.fc3 = nn.Linear(3, 1)
  def forward(self, x):
    x = F.relu(self.fc1(x))
    x = F.relu(self.fc2(x))
    return torch.sigmoid(self.fc3(x))
net = Net(X_train.shape[1])

criterion = nn.BCELoss()


optimizer = optim.Adam(net.parameters(), lr=0.001)


def calculate_accuracy(y_true, y_pred):
  predicted = y_pred.ge(.5).view(-1)
  return (y_true == predicted).sum().float() / len(y_true)

    
MODEL_PATH = 'model.pth'
torch.save(net, MODEL_PATH)    

net = torch.load(MODEL_PATH)



