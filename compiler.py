'''
Louielee Own Compiler (.loc)
'''

import sys

program_filepath = sys.argv[1]

#read arguments
print(program_filepath)

print("[CMD] Parsing")


#Tokenize program

#Read file lines
program_lines = []
with open(program_filepath, "r" ) as program_file:
    program_lines = [
        line.strip()
            for line in program_file.readlines()
    ]

program = []
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    if opcode == "":
        continue

    program.append(opcode)

    if opcode == "PUSH" :
        number = int(parts[1])
        program.append(number)
    elif opcode =="PRINT":
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
    elif opcode == "JUMP.EQ.0":
        label = parts[1]
        program.append(label)
    elif opcode == "JUMP.GT.0":
        label = parts[1]
        program.append(label)

print(program)