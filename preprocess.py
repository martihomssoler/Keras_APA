

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
native-country
"""
workclass= ['Private','Self-emp-not-inc','Self-emp-inc','Federal-gov', 'Local-gov', 'State-gov', 'Without-pay', 'Never-worked','?']
native_country=['United-States','Cambodia','England','Puerto-Rico','Canada','Germany','Outlying-US(Guam-USVI-etc)','India','Japan','Greece','South','China','Cuba','Iran','Honduras','Philippines','Italy','Poland','Jamaica','Vietnam','Mexico','Portugal','Ireland','France','Dominican-Republic','Laos','Ecuador','Taiwan','Haiti','Columbia','Hungary','Guatemala','Nicaragua','Scotland','Thailand','Yugoslavia','El-Salvador','Trinadad&Tobago','Peru','Hong','Holand-Netherlands','?']
sex=['Male','Female','?']
education=['Bachelors','Some-college','11th','HS-grad','Prof-school','Assoc-acdm','Assoc-voc','9th','7th-8th','12th','Masters','1st-4th','10th','Doctorate','5th-6th','Preschool','?']
marital_status= ['Married-civ-spouse','Divorced','Never-married','Separated','Widowed','Married-spouse-absent','Married-AF-spouse','?']
occupation=['Tech-support','Craft-repair','Other-service','Sales','Exec-managerial','Prof-specialty','Handlers-cleaners','Machine-op-inspct','Adm-clerical','Farming-fishing','Transport-moving','Priv-house-serv','Protective-serv','Armed-Forces','?']
relationship=['Wife','Own-child','Husband','Not-in-family','Other-relative','Unmarried','?']
race= ['White','Asian-Pac-Islander','Amer-Indian-Eskimo','Other','Black','?']

#North america=0
#south america=1
#Asia=2
#Europa=3
#Euroasia=4
#Other=5

#Notmarried=0
#Married=1
#widowed=2
#Nevermarried=3
#Other=4

#NotFinished=0
#Bachelors=1
#Master=2
#Doctorate=3
#Associates=4
#Graduates=5
#Others=6

def selectCountry(data):
    i=5
    if(data=="United-States" or data=="Canada"):
        i=0#North america
    elif(data=="Columbia" or data=="Cuba" or data=="Dominican-Republic" or data=="Ecuador" or data=="El-Salvador" or data=="Guatemala" or data=="Haiti" or data=="Honduras" or data=="Jamaica" or data=="Mexico" or data=="Peru" or data=="Puerto-Rico" or data=="Trinadad&Tobago" or data=="Nicaragua" or data=="Outlying-US(Guam-USVI-etc)"):
        i=1
    elif(data=="Cambodia" or data=="China" or data=="Japan" or data=="Hong" or data=="Philippines" or data=="Laos" or data=="Taiwan" or data=="Thailand" or data=="Vietnam" or data=="India"):
        i=2
    elif(data=="England" or data=="France" or data=="Germany" or data=="Holand-Netherlands" or data=="Ireland" or data=="Italy" or data=="Poland" or data=="Scotland" or data=="Portugal"):
        i=3
    elif(data=="Yugoslavia" or data=="South" or data=="Hungary" or data=="Greece" or data=="Iran"):
        i=4
    return i

def selectMarital(data):
	i=4
	if(data=="Divorced" or data=="Separated"):
		i=0
	elif(data=="Married-civ-spouse" or data=="Married-spouse-absent" or data=="Married-AF-spouse"):
		i=1
	elif(data=="Widowed"):
		i=2
	elif(data=="Never-married"):
		i=3
	return i


def selectEducation(data):
	i=6
	if(data=="11th" or data=="9th" or data=="7th-8th" or data=="12th" or data=="5th-6th" or data=="1st-4th" or data=="Preschool" or data=="10th"):
		i=0
	elif(data=="Bachelors"):
		i=1
	elif(data=="Masters"):
		i=2
	elif(data=="Doctorate"):
		i=3
	elif(data=="Assoc-acdm" or data=="Assoc-voc"):
		i=4
	elif(data=="?"):
		i=6
	else:
		i=5
	return i
	

def process(data):
	for i in range(0,len(data)):
		if i==1:
			data[i]=workclass.index(data[i])
		elif i==3:
			data[i]=selectEducation(data[i])
		elif i==5:
			data[i]=selectMarital(data[i])
		elif i==6:
			data[i]=occupation.index(data[i])
		elif i==7:
			data[i]=relationship.index(data[i])
		elif i==8:
			data[i]=race.index(data[i])
		elif i==9:
			data[i]=sex.index(data[i])
		elif i==13:
			data[i]=selectCountry(data[i])
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
		print(data)

main()
