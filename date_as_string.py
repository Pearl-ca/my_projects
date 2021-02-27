
def date_as_string_with_parameters(day,month,year):
    name_of_month=what_month(month)
    print(name_of_month," ", day, " ", year)
    

def what_month(month):
  if month <1 or month >12:
      print('The number of a month should be between 1 and 12')
  elif month==1:
      month_name="January"
      return month_name
  elif month==2:
      month_name="February"
      return month_name
  elif month==3:
      month_name="March"
      return month_name
  elif month==4:
      month_name="April"
      return month_name
  elif month==5:
      month_name="May"
      return month_name
  elif month==6:
      month_name="June"
      return month_name
  elif month==7:
      month_name="July"
      return month_name
  elif month==8:
      month_name="August"
      return month_name
  elif month==9:
      month_name="September"
      return month_name
  elif month==10:
      month_name="Otober"
      return month_name
  elif month==11:
      month_name="November"
      return month_name
  elif month==12:
      month_name="December"
      return month_name

try:
    day=int(input("Enter a day: "))
    month=int(input("Enter a number of month: "))
    year=int(input("Enter a year: "))
        
    date_as_string_with_parameters(day,month,year)
except:
        print("Enter Appropriate Numbers")
    
