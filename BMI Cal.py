def stature_by_gender():
    
    if gender=='Female':
        if height<1.62:
            return 'Short in stature'
        elif height<1.72:
            return 'Average height'
        elif height>=1.72 or height<2.50:
            return 'Tall in stature'  
    elif gender=='Male':
        if height<1.76:
            return 'Short in stature '
        elif height<1.86:
            return 'Average height'
        elif height>=1.86 or height<2.84:
            return 'Tall in stature'
    
def Body_Mass(a):       
    if BMI<=18.5:
       print(stature_by_gender(),'and Underweight')
    elif BMI>18.5 and BMI<=25.0:
        print(stature_by_gender(),'and Normal')
    elif BMI>25.0 and BMI<30:
        print(stature_by_gender(),'and Overweight')
    elif BMI>=30:
         print(stature_by_gender(),'and Obese')

while True:       
    try:
        height=input('Enter height in meters:')
        height=float(height)
        break
    except:
        print("Please use reasonable heights")
while True:        
    try:
        weight=input('Enter weight in kilograms:')
        weight=float(weight)
        break
    except:
        print('Please use actual weights')
while True:
        gender=input('State gender Male/Female:')
        if gender == "Male" or gender == "Female":
            break
        else:
            print("gender must be Male/Female")
BMI = round((weight/height**2),2)
Body_Mass(BMI)


