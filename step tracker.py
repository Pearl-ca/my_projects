def num_of_healthy(x):
    c=0
    for h in x:
        if int(h) > 10000: 
            c+=1
    return (c)

filename=input("Please enter the filename: ")

try:
    fhandle=open(filename)
except:
    print(filename," not found.")
    
    
steps_list=[]
for line in fhandle:
    line=line.strip()
    steps_list.append(line)
    
#calculating the total of the steps taken so far    
total=0
i=0
while i < len(steps_list):
    total += int(steps_list[i])
    i +=1

print("The total number of steps: " + str(total))

#invokes the num_of_healthy function to get the productive work outs
print("Healthy days: "+ str(num_of_healthy(steps_list)))
    
try:
    today=input("Enter today's number of steps: ")
    if int(today) > 10000:
        print("You're on track on your daily schedule and don't need to jog more today :)")
    else:
        rem=10000-int(today)
        print("You have to take at least " + str(rem) + " steps more today.")
except:
    print("Please enter integer values.")
