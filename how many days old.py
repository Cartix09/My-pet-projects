#Age Calculator: Discover Your Age in Days

from datetime import datetime

def calculate_age_in_days(birth_date_str=None):
    if birth_date_str is None:
        birth_date_str = input("Enter the date of birth in format mm/dd/yyyy: ")
        
    try:
        birth_date = datetime.strptime(birth_date_str, "%m/%d/%Y")
        current_date = datetime.now()
        if current_date < birth_date:
            print("The birth date is in the future. Please enter a valid past date.")
            return
        age_in_days = (current_date - birth_date).days
        print(f"You are {age_in_days} days old today.")
    except ValueError:
        print("Invalid date format. Please enter the date in mm/dd/yyyy format.")

calculate_age_in_days()

