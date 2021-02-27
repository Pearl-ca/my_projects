try:
    file = open('data.txt')
except:
    print('File cannot be opened:')
    quit()

nestlist=[]
names=[]
heights=[]
weights=[]
#name~height in metres~weight in kg
for line in file:
    line=line.strip()
    line=line.split('~')
    nestlist.append(line)
    names.append(line[0])
    heights.append(float(line[1]))
    weights.append(int(line[2]))
    
print(nestlist,"\n")
print(names,"\n")
print(heights,"\n")
print(weights,"\n")



def Body_Mass(BMI):
    if BMI<=18.5:
        return('Underweight')
    elif BMI>18.5 and BMI<=25.0:
        return('Normal')
    elif BMI>25.0 and BMI<30:
        return('Overweight')
    elif BMI>=30:
        return('Obese')
mass=[]

#BMI = round((weight/height**2),2)
for item in nestlist:

    BMI =round((float(item[2].strip())/(float(item[1].strip())*float(item[1].strip()))),2)
    mass.append(Body_Mass(BMI))
print(mass)

def find_category(text):
    text= text.title()
    i=0
    your_list=[]
    while i < len(mass):
        print()
        if mass[i] == text:
            your_list.append(names[i])
        i+=1
    return(your_list)


Normal=[]
Underweight=[]
Overweight=[]
Obese=[]
for i in mass:
    if i=='Normal':
        Normal.append(i)
    if i=='Underweight':
        Underweight.append(i)
    elif i=='Overweight':
        Overweight.append(i)
    elif i=='Obese':
        Obese.append(i)
        
# find_category("Normal")
from easygui import *
category=enterbox('what BMI category (Normal/Underweight/Overweight/Obese) are you interested in?')
    
if category=='' or category==None:
    msgbox("you have to choose a category")
elif category.title()=='Normal':
    i=1
    mystring=''
    for item in find_category(category):
        mystring= mystring + ' ' + str(i) + ' '   + item + '\n'
        i+=1
    msgbox(mystring)
elif category.title()=='Underweight':
    i=1
    mystring=''
    for item in find_category(category):
        mystring= mystring + ' ' + str(i) + ' '   + item + '\n'
        i+=1
    msgbox(mystring)
elif category.title()=='Overweight':
    i=1
    mystring=''
    for item in find_category(category):
        mystring= mystring + ' ' + str(i) + ' '   + item + '\n'
        i+=1
    msgbox(mystring)
elif category.title()=='Obese':
    i=1
    mystring=''
    for item in find_category(category):
        mystring= mystring + ' ' + str(i) + ' '   + item + '\n'
        i+=1
    msgbox(mystring)

    