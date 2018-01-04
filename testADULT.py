import keras
from keras.models import Sequential
from keras.layers import Dense, Flatten, Reshape
from keras import backend as K
import numpy as np
import re
import requests as R
import json

"""
age: continuous.
workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
fnlwgt: continuous.
education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
education-num: continuous.
marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
sex: Female, Male.
capital-gain: continuous.
capital-loss: continuous.
hours-per-week: continuous.
native-country"""
workclass= ['Private','Self-emp-not-inc','Self-emp-inc','Federal-gov', 'Local-gov', 'State-gov', 'Without-pay', 'Never-worked','?']
native_country=['United-States','Cambodia','England','Puerto-Rico','Canada','Germany','Outlying-US(Guam-USVI-etc)','India','Japan','Greece','South','China','Cuba','Iran','Honduras','Philippines','Italy','Poland','Jamaica','Vietnam','Mexico','Portugal','Ireland','France','Dominican-Republic','Laos','Ecuador','Taiwan','Haiti','Columbia','Hungary','Guatemala','Nicaragua','Scotland','Thailand','Yugoslavia','El-Salvador','Trinadad&Tobago','Peru','Hong','Holand-Netherlands','?']
sex=['Male','Female','?']
education=['Bachelors','Some-college','11th','HS-grad','Prof-school','Assoc-acdm','Assoc-voc','9th','7th-8th','12th','Masters','1st-4th','10th','Doctorate','5th-6th','Preschool','?']
marital_status= ['Married-civ-spouse','Divorced','Never-married','Separated','Widowed','Married-spouse-absent','Married-AF-spouse','?']
occupation=['Tech-support','Craft-repair','Other-service','Sales','Exec-managerial','Prof-specialty','Handlers-cleaners','Machine-op-inspct','Adm-clerical','Farming-fishing','Transport-moving','Priv-house-serv','Protective-serv','Armed-Forces','?']
relationship=['Wife','Own-child','Husband','Not-in-family','Other-relative','Unmarried','?']
race= ['White','Asian-Pac-Islander','Amer-Indian-Eskimo','Other','Black','?']


def process(data):
	for i in range(0,len(data)):
		if i==1:
			data[i]=workclass.index(data[i])
		elif i==3:
			data[i]=education.index(data[i])
		elif i==5:
			data[i]=marital_status.index(data[i])
		elif i==6:
			data[i]=occupation.index(data[i])
		elif i==7:
			data[i]=relationship.index(data[i])
		elif i==8:
			data[i]=race.index(data[i])
		elif i==9:
			data[i]=sex.index(data[i])
		elif i==13:
			data[i]=native_country.index(data[i])
		elif i==14:
			if data[i]=='<=50K':
				data[i]=0
			elif data[i]=='>50K':
				data[i]=1
		else:
			data[i]=int(data[i])
	return data

def main():
	x_train=[]
	y_train=[]
	adult_file=open("adult.data",'r')
	for line in adult_file:
		#line=adult_file.readline()
		data = line.replace(' ','').rstrip().split(",")
		data=process(data)
		x_train.append(data[:len(data)-1])
		y_train.append(data[-1])
	x_train=np.array(x_train)
	print (y_train[7])
	y_train=np.array(y_train)
	model = Sequential()
	model.add(Dense(64, activation='relu', input_shape=(14,)))
	model.add(Dense(32, activation='relu'))
	model.add(Dense(16, activation='relu'))
	model.add(Dense(1, activation='softmax'))

	model.compile(loss='binary_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

	model.fit(x_train, y_train,
	          epochs=20,
	          batch_size=128,
	          verbose=1)

	x_test = np.array([x_train[7]])
	print (x_test)
	y_test = np.array([y_train[7]])
	# evaluate loaded model on test data
	#loaded_model.compile(loss='binary_crossentropy', optimizer='rmsprop', metrics=['accuracy'])
	score = model.evaluate(x_test, y_test, verbose=0)
	print("%s: %.2f" % (model.metrics_names[0], score[0]*100))
	print("%s: %.2f%%" % (model.metrics_names[1], score[1]*100))

main()