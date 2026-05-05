import random

def generate_master_file(filename, num_accounts):
    with open(filename, "w") as f:
        for i in range(1, num_accounts + 1):
            account_number = str(i).zfill(10)
            name = f"Customer{i}".ljust(20)
            balance = str(random.randint(1000, 500000)).zfill(10)
            account_type = random.choice(["C", "S"])
            status = "A"

            record = account_number + name + balance + account_type + status
            f.write(record + "\n")

generate_master_file("master.dat", 100)