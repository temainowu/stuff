from dataclasses import dataclass


@dataclass
class Users:
    name: str = ""
    age: int = 0
    mark: int = 0


userData = []

file = open('Data.txt', 'r')
data = file.readlines()
file.close()

for line in data:
    name, age, mark = line.strip().split(", ")
    userData.append(
        Users(name, int(age), int(mark))
    )

print(userData)
