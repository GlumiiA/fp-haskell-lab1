with open("0022_names.txt", "r") as f:
    content = f.read()

names = content.replace('"', '').split(',')

names.sort()

def name_value(name):
    return sum(ord(char) - ord('A') + 1 for char in name)

total_score = sum((i + 1) * name_value(name) for i, name in enumerate(names))

print(total_score)
