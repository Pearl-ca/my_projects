#Take a score as an integer if not int, then error message

try:
    score=int(input("What is your score: "))
    if score > 100 or score < 0:
        print("Please enter a score between 0 and 100")
    try:
        grade_type=int(input("Is your grade differentiated. Enter 1 if yes, and 0 if no: "))
    
        if grade_type==0:
            if score >50:
                print("You have passed the course.")
            else:
                print("You have failed the course.") 
        elif grade_type==1:
            if score >=91:
                print("You have got an A")
            elif score >=81:
                print("You have got a B")
            elif score >=71:
                print("You have got a C")
            elif score >=61:
                print("You have got a D")
            elif score >=51:
                print("You have got an E")
            else:
                print("You have gotten an F")
        else:
            print("Please enter either a 0 or a 1.")
    except:
        print("Please enter either a 0 or a 1.")
except:
    print("Please enter an integer.")

#Ask for grade differentiation type
    


