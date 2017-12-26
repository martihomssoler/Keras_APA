import keras
from keras.models import Sequential
from keras.layers import Dense, Flatten, Reshape
from keras import backend as K
import numpy as np

import requests as R
import json

import OpenDotaRequests as odr
# raw_dataset_train = np.loadtxt("dota2Train.csv", delimiter=",")
# x_train = keras.utils.to_categorical(raw_dataset_train[:,4:(len(raw_dataset_train))], num_classes=3).reshape(len(raw_dataset_train),113,3)
# y_train = raw_dataset_train[:,0]

x_train, y_train = get_next_info(20)
x_train = np.zeros(shape=(20,10,len(odr.KEYS)))
y_train = np.zeros(shape=(20,1)) 
for i in range(0,20):
	x, y = odr.get_info()
	x_train[i] = x
	y_train[i] = y

print(x_train)
# 	match_data = match.json()
# 	x_train.append(np.array(list(match_data.values())))
# 	y_train.append(np.bool_(match_data['radiant_win']))
#x_train = np.array(x_train)
#y_train = np.array(y_train)

model = Sequential()
model.add(Dense(64, activation='relu', input_shape=(10,)))
model.add(Dense(32, activation='relu'))
model.add(Dense(16, activation='relu'))
model.add(Dense(1, activation='softmax'))

model.compile(loss='binary_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

# model = Sequential()
# model.add(Dense(63, activation='relu', input_shape=(113,3,)))
# model.add(Dense(32, activation='relu'))
# model.add(Flatten())
# model.add(Dense(1, activation='softmax'))

# model.compile(loss='binary_crossentropy',
#               optimizer='rmsprop',
#               metrics=['accuracy'])

model.fit(x_train, y_train,
          epochs=20,
          batch_size=128,
          verbose=1)

# serialize model to YAML
# model_yaml = model.to_yaml()
# with open("model.yaml", "w") as yaml_file:
#     yaml_file.write(model_yaml)
# # serialize weights to HDF5
# model.save_weights("model.h5")
# print("Saved model to disk")

# # load YAML and create model
# yaml_file = open('model.yaml', 'r')
# loaded_model_yaml = yaml_file.read()
# yaml_file.close()
# loaded_model = keras.models.model_from_yaml(loaded_model_yaml)
# # load weights into new model
# loaded_model.load_weights("model.h5")
# print("Loaded model from disk")

# raw_dataset_test = np.loadtxt("dota2Test.csv", delimiter=",")
# x_test = keras.utils.to_categorical(raw_dataset_test[:,4:(len(raw_dataset_test))], num_classes=3).reshape(len(raw_dataset_test),113,3)
# y_test = raw_dataset_test[:,0]

match = R.get('https://api.opendota.com/api/matches/' + str(data[1]['match_id']))
match_data = match.json()
x_test = match_data
y_test = match_data['radiant_win']

# evaluate loaded model on test data
loaded_model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
score = loaded_model.evaluate(x_test, y_test, verbose=0)
print("%s: %.2f" % (loaded_model.metrics_names[0], score[0]*100))
print("%s: %.2f%%" % (loaded_model.metrics_names[1], score[1]*100))