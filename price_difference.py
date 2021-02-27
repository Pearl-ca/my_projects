#Installation decision making program

def price_difference(init_pay,monthly_pay,num_month):
    return (monthly_pay * num_month) - init_pay

try:
    product_price=float(input("What is the price of the product? "))
    
    #first installment plan
    monthly1=float(input("What is the monthly payment on the first installment plan? "))
    month_count1=int(input("How many months does the first installment plan last? "))
    
    #second installment plan
    monthly2=float(input("What is the monthly payment on the second installment plan? "))
    month_count2=int(input("How many months does the second installment plan last? "))
    
    try:
        first_plan=price_difference(product_price,monthly1,month_count1)
        second_plan=price_difference(product_price,monthly2,month_count2)

        if first_plan < second_plan:
            print("The first installment plan is better.")
        else:
            print("The second installment plan is better.")
    except:
        print("Couldn't invoke the function")
        
except (ValueError, NameError):
    print("Please enter appropraite numbers.")




