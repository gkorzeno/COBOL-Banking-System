import random
from datetime import datetime, timedelta

def generate_transaction_file(filename, num_transactions, max_account):
    start_date = datetime(2025, 1, 1)
    end_date = datetime(2025, 12, 31)
    days_between = (end_date - start_date).days

    transactions = []

    for i in range(1, num_transactions + 1):
        account_number = str(random.randint(1, max_account)).zfill(10)
        transaction_id = str(i).zfill(8)

        txn_type = random.choice(["D", "W"])
        amount = str(random.randint(100, 20000)).zfill(10)

        random_days = random.randint(0, days_between)
        random_datetime = start_date + timedelta(days=random_days,
                                                  hours=random.randint(0, 23),
                                                  minutes=random.randint(0, 59),
                                                  seconds=random.randint(0, 59))

        date_str = random_datetime.strftime("%Y%m%d")   # 8 chars
        time_str = random_datetime.strftime("%H%M%S")   # 6 chars

        record = (
            account_number +
            transaction_id +
            date_str +
            time_str +
            txn_type +
            amount
        )

        transactions.append(record)

    # Sort by account number, then date/time
    transactions.sort(key=lambda r: (r[0:10], r[18:32]))

    with open(filename, "w") as f:
        for record in transactions:
            f.write(record + "\n")

generate_transaction_file("transactions.dat", 1000, 100)