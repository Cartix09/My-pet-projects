#BMI calculator v2
#HealthMate: Personal BMI and Wellness Tracker

def calculate_bmi(height, weight):
    return round(weight / (height ** 2), 2)

def bmi_category(bmi):
    if bmi <= 16:
        return "very underweight"
    elif bmi <= 18.5:
        return "underweight"
    elif bmi <= 25:
        return "healthy"
    elif bmi <= 30:
        return "overweight"
    else:
        return "very overweight"

def main():
    try:
        height = float(input("Enter your Height in Meters 'x.xx': ")) #for example 1.89 and not 189
        weight = float(input("Enter your Weight in Kilograms: "))
        age = int(input("Enter your Age: "))
        
        if height <= 0 or weight <= 0:
            print("Height and Weight must be positive numbers.")
            return

        if age < 0:
            print("Age cannot be negative.")
            return

        bmi = calculate_bmi(height, weight)
        category = bmi_category(bmi)

        print(f"BMI Calculated is: {bmi}")
        print(f"Based on your BMI, you are {category}")

    except ValueError:
        print("Please enter valid numerical values for height, weight, and age.")

if __name__ == "__main__":
    main()
