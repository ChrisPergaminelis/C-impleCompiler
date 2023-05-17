import copy, sys
# Agamemnon Kyriazis 4400
# Chris Pergaminelis 4474
class Token:

    def __init__(self, family, recognized_string, line_number):
        self.family = family
        self.recognized_string = recognized_string
        self.line_number = str(line_number)

    def __str__(self):
        return self.recognized_string + "\t" + self.family + "\t" + self.line_number


class LexicalAnalyzer:

    cursor = 0

    char_stream = list()

    current_line = 0

    token_list = list()

    input_encoding = {"blank": 0, "digit": 1, "letter": 2, "addOperator": 3, "multOperator": 4, "groupSymbol": 5,
                      "delimiter": 6,
                      "colonSign": 7, "lessSign": 8, "greaterSign": 9, "equalsSign": 10, "commentSign": 11, "eof": 12,
                      "other": 13}

    start = ["start", "dig", "idk", "addOperator", "multOperator", "groupSymbol", "delimiter", "asgn", "smaller",
             "larger", "relOperator", "comment", "eof", "er"]

    dig = ["number", "dig", "er", "number", "number", "number", "number", "number", "number", "number", "number",
           "number", "er", "number"]

    idk = ["identifier", "idk", "idk", "identifier", "identifier", "identifier", "identifier", "identifier",
           "identifier", "identifier", "identifier", "identifier", "er", "identifier"]

    asgn = ["er", "er", "er", "er", "er", "er", "er", "er", "er", "er", "assignment", "er", "er", "er"]

    smaller = ["relOperator", "relOperator", "relOperator", "er", "er", "relOperator", "er", "er", "er", "relOperator",
               "relOperator", "er", "er", "relOperator"]

    larger = ["relOperator", "relOperator", "relOperator", "er", "er", "relOperator", "er", "er", "er", "er",
              "relOperator", "er", "er", "relOperator"]

    comment = ["comment", "comment", "comment", "comment", "comment", "comment", "comment", "comment", "comment",
               "comment", "comment", "endComment", "er", "comment"]

    state_encoding = {"start": 0, "dig": 1, "idk": 2, "asgn": 3, "smaller": 4, "larger": 5, "comment": 6}

    final_states = ["addOperator", "multOperator", "groupSymbol", "delimiter", "relOperator", "endComment", "number",
                    "identifier", "assignment", "er", "eof"]

    state_table = [start, dig, idk, asgn, smaller, larger, comment]

    keywords = ["program", "declare", "if", "else", "while", "switchcase", "forcase", "incase", "case", "default",
                "not", "and", "or", "function",
                "procedure", "call", "return", "in", "inout", "input", "print"]

    def __init__(self, file_name):
        self.file_name = file_name
        self.program_file = open(file_name, "r+", encoding='utf-8')

    def error(self, message):
        print("Lexical error, " + message)
        self.program_file.close()
        exit(1)

    def get_char_type(self, char):
        if char == '':
            return "eof"
        if char.isspace():
            return "blank"
        if char.isalpha():
            return "letter"
        if char.isdigit():
            return "digit"
        if char in "+-":
            return "addOperator"
        if char in "*/":
            return "multOperator"
        if char in "{([}])":
            return "groupSymbol"
        if char in ";,.":
            return "delimiter"
        if char == ":":
            return "colonSign"
        if char == "<":
            return "lessSign"
        if char == ">":
            return "greaterSign"
        if char == "=":
            return "equalsSign"
        if char == "#":
            return "commentSign"
        else:
            return "other"

    def recognize_next_token(self):
        state = "start"
        recognized_string = str()
        while state not in self.final_states:
            next_char = self.char_stream[self.cursor]
            if next_char == '\n':
                self.current_line += 1
            self.cursor += 1
            state = self.state_table[self.state_encoding.get(state)][self.input_encoding.get(self.get_char_type(next_char))]
            if state != "start":
                recognized_string += next_char
        if state in ["identifier", "number"]:
            self.cursor -= 1
            if self.char_stream[self.cursor] == "\n":
                self.current_line -= 1
            recognized_string = recognized_string[:-1]
        if state == "identifier" and recognized_string in self.keywords:
            state = "keyword"
        if state == "relOperator" and recognized_string not in ["<=", ">=", "=", "<>"]:
            self.cursor -= 1
            if self.char_stream[self.cursor] == "\n":
                self.current_line -= 1
            recognized_string = recognized_string[:-1]
        if state == "identifier" and len(recognized_string) > 30:
            self.error("V.L.I. (Very Large Identifier) above 30 characters error")
        if state == "number" and (int(recognized_string) > (2**32 - 1) or int(recognized_string) < -(2**32 - 1)):
            self.error("V.L.N. (Very Large Number) greater that 2^32 error or less than -2^32")
        return state, recognized_string, self.current_line+1

    def perform_lexical_analysis(self):
        self.char_stream = list(self.program_file.read())
        #print(self.char_stream)
        self.char_stream.append('')
        while True:
            family, recognized_string, line_number = self.recognize_next_token()
            if family == "er":
                self.error(f"at line:{self.current_line}, encountered {recognized_string}")
            if family == "endComment":
                continue
            if family == "eof":
                self.token_list.append(Token(family, "eof", line_number))
                break
            self.token_list.append(Token(family, recognized_string, line_number))
        self.token_list.reverse()
        #print(f"Lexical analysis successful. Total lines parsed: {self.current_line+1}")
        #[print(token) for token in self.token_list]

    def pop_next_token(self) -> Token:
        return self.token_list.pop()

    def get_next_token(self) -> Token:
        return self.token_list[-1]


class BoolList:

    def __init__(self):
        self.true = []
        self.false = []

    def __str__(self):
        return "True: " + str(self.true) + "\n" \
                                           "False: " + str(self.false)



class Quad:

    def __init__(self, operator, operand1, operand2, operand3):
        self.operator = operator
        self.operand1 = operand1
        self.operand2 = operand2
        self.operand3 = operand3

    def __str__(self):
        return str(self.operator) + ", " + str(self.operand1) + ", " + str(self.operand2) + ", " + str(self.operand3)


class Variable:

    def __init__(self, name, dataType, offset):
        self.name = name
        self.dataType = dataType
        self.offset = offset

    def __str__(self):
        return self.name + "/" + str(self.offset)



class TemporaryVariable(Variable):

    def __init__(self, name, dataType, offset):
        Variable.__init__(self, name, dataType, offset)



class FormalParameter:

    def __init__(self, name, dataType, mode):
        self.name = name
        self.dataType = dataType
        self.mode = mode

    def __str__(self):
        return self.name + " " + self.mode



class Parameter(FormalParameter, Variable):

    def __init__(self, name, dataType, mode, offset):
        Variable.__init__(self, name, dataType, offset)
        FormalParameter.__init__(self, name, dataType, mode)

    def __str__(self):
        return self.name + "/" + str(self.offset) + " " + self.mode



class Procedure:

    def __init__(self, name, startingQuad, frameLength):
        self.name = name
        self.startingQuad = startingQuad
        self.frameLength = frameLength
        self.formalParameters = list()

    def __str__(self):
        return self.name + "/" + str(self.frameLength) + " " + str([formalPar.__str__() for formalPar in self.formalParameters]) + "(" + str(self.startingQuad) + ")"

    def addFormalParameter(self, parName, parMode):
        formalPar = FormalParameter(parName, "integer", parMode)
        self.formalParameters.append(formalPar)

    def updateStartingQuad(self, quad):
        self.startingQuad = quad

    def updateFrameLength(self, frameLength):
        self.frameLength = frameLength



class Function(Procedure):

    def __init__(self, name, dataType, startingQuad, frameLength):
        Procedure.__init__(self, name, startingQuad, frameLength)
        self.datatype = dataType
        self.formalParameters = list()

    def __str__(self):
        return self.name + "/" + str(self.frameLength) + " " + str([formalPar.__str__() for formalPar in self.formalParameters]) + "(" + str(self.startingQuad) + ")"

    def addFormalParameter(self, parName, parMode):
        formalPar = FormalParameter(parName, "integer", parMode)
        self.formalParameters.append(formalPar)

    def updateStartingQuad(self, quad):
        self.startingQuad = quad

    def updateFrameLength(self, frameLength):
        self.frameLength = frameLength



class IntermediateCode:

    def __init__(self):
        self.intermediate_code = list()
        self.quad_counter = 1
        self.temp_counter = 0
        self.int_code_file = open('a.int', 'w')

    def __str__(self):
        retStr = str()
        for quadId, quad in self.intermediate_code:
            retStr += f'L{quadId},' + quad.__str__() + '\n'
        return retStr

    def empty_list(self):
        print('emtpylist')
        return []

    def make_list(self, label):
        print('makelist', '[', label, ']')
        return [label]

    def merge(self, list1, list2):
        print('merge(', list1, list2, ')')
        return list1 + list2

    def gen_quad(self, op, x, y, z):
        quad = Quad(op, x, y, z)
        print(f'genQuad({quad.operator}, {quad.operand1}, {quad.operand2}, {quad.operand3})')
        self.intermediate_code.append((self.quad_counter, quad))
        self.quad_counter += 1
        return quad

    def backpatch(self, list_obj, label):
        print('backpatch', list_obj, label)
        for ptr in list_obj:
            self.intermediate_code[int(ptr)-1][1].operand3 = f'L{label}'

    def next_quad(self):
        print('nextQuad', self.quad_counter)
        return self.quad_counter

    def new_temp(self):
        self.temp_counter += 1
        tempVar = f"T_{self.temp_counter}"
        return tempVar

    def write_int_file(self):
        #print('Writing intermediate code to a.int...')
        self.int_code_file.write(self.__str__())

    def save_int_file(self):
        #print('Saving a.int...')
        self.int_code_file.flush()
        self.int_code_file.close()



class Scope:

    def __init__(self, level):
        self.level = level
        self.entities = list()
        self.returnAddress = None  # 4
        # we don't actually use this
        self.accessLink = None  # +4
        # we don't actually use this
        self.returnValue = None  # +4
        # we don't actually use this
        self.offset = 12

    def __str__(self):
        return str(self.level) + " : " + f"({self.returnAddress}|{self.accessLink}|{self.returnValue})" + " ".join([entity.__str__() for entity in self.entities])

    def addProcedure(self, procName):
        proc = Procedure(procName, None, None)
        self.entities.append(proc)

    def addFunction(self, funcName):
        func = Function(funcName, "integer", None, None)
        self.entities.append(func)

    def addVariable(self, varName):
        var = Variable(varName, "integer", self.offset)
        self.entities.append(var)
        self.offset += 4

    def addTempVariable(self, varName):
        tempVar = TemporaryVariable(varName, "integer", self.offset)
        self.entities.append(tempVar)
        self.offset += 4

    def addFormalParameter(self, parName, parMode):
        self.entities[-1].addFormalParameter(parName, parMode)

    def addParameter(self, parName, parMode):
        par = Parameter(parName, "integer", parMode, self.offset)
        self.entities.append(par)
        self.offset += 4

    def updateStartingQuad(self, quad):
        self.entities[-1].updateStartingQuad(quad)

    def updateFrameLength(self, frameLength):
        self.entities[-1].updateFrameLength(frameLength)

    def searchByName(self, entityName):
        for entity in self.entities:
            if entity.name == entityName:
                return entity
        else:
            return None



class Table:

    def __init__(self, scopes=list(), levelCounter=0):
        self.scopes = copy.deepcopy(scopes)
        self.levelCounter = levelCounter
        self.symbol_table_file = None

    def __str__(self):
        return "\n".join([scope.__str__() for scope in self.scopes[::-1]])

    def error(self, message):
        print('Symbol table error,', message)
        exit(1)

    def addLevel(self):
        self.scopes.append(Scope(self.levelCounter))
        self.levelCounter += 1

    def popLevel(self):
        self.levelCounter -= 1
        scope = self.scopes.pop()
        return scope

    def addProcedure(self, procName):
        self.scopes[self.levelCounter-1].addProcedure(procName)

    def addFunction(self, funcName):
        self.scopes[self.levelCounter-1].addFunction(funcName)

    def addVariable(self, varName):
        self.scopes[self.levelCounter-1].addVariable(varName)

    def addTempVariable(self, varName):
        self.scopes[self.levelCounter-1].addTempVariable(varName)

    def addFormalParameter(self, parName, parMode):
        self.scopes[self.levelCounter-1].addFormalParameter(parName, parMode)

    def addParameter(self, parName, parMode):
        self.scopes[self.levelCounter-1].addParameter(parName, parMode)

    def updateStartingQuad(self, quad):
        self.scopes[self.levelCounter-2].updateStartingQuad(quad)

    def updateFrameLength(self, frameLength):
        self.scopes[self.levelCounter-2].updateFrameLength(frameLength)

    def searchByName(self, entityName):
        scope = self.scopes[-1]
        entity = scope.searchByName(entityName)
        while entity is None:
            scope = self.scopes[scope.level - 1]
            entity = scope.searchByName(entityName)
            if scope.level == 0 and entity is None:
                self.error(f'variable {entityName} not found')
        return scope, entity

    def open_symb_file(self):
        self.symbol_table_file = open('a.symb', 'w')

    def write_symb_file(self):
        #print('Writing symbol table snapshot to a.symb...')
        self.symbol_table_file.write(self.__str__())
        self.symbol_table_file.write('\n'+'#'*100+'\n')

    def save_symb_file(self):
        #print('Saving a.symb...')
        self.symbol_table_file.flush()
        self.symbol_table_file.close()

    def getTable(self, table):
        return table



class TableManager:

    def __init__(self):
        self.symbolTableList = list()

    def addSnapshot(self, tableInstance: Table):
        self.symbolTableList.append(tableInstance)

    def popSnapshot(self):
        self.symbolTableList = self.symbolTableList[1:]
        return self.symbolTableList

    def getLatestSnapshot(self):
        return self.symbolTableList[0]



class AssemblyCode:

    operatorToOperationDecoder = {'+': 'add', '-': 'sub', '*': 'mul', '/': 'div'}
    relOperatorToOperationDecoder = {'=': 'beq', '<>': 'bne', '<': 'blt', '>': 'bgt', '<=': 'ble', '>=': 'bge'}

    def __init__(self, symbolTable: Table, intermediateCode: IntermediateCode, symbolTableManager: TableManager):
        self.symbolTable = symbolTable
        self.intermediateCode = intermediateCode
        self.symbolTableManager = symbolTableManager
        self.assemblyCode = list()
        self.assemblyCodeFile = open('a.asm', 'w')
        self.produce('.data')
        self.produce('str_nl: .asciz "\\n"')
        self.produce('.text')

    def __str__(self):
        return '\n'.join([line for line in self.assemblyCode])

    def produce(self, codeLine, pos=-1):
        if pos == -1:
            self.assemblyCode.append(codeLine)
        else:
            self.assemblyCode.insert(pos, codeLine)

    def gnlvcode(self, v):
        scope, entity = self.symbolTable.searchByName(v)
        currentLevel = self.symbolTable.levelCounter-1
        targetLevel = scope.level
        n = currentLevel-targetLevel
        self.produce('lw t0, -4(sp)')
        n = n-1
        while n > 0:
            self.produce('lw t0, -4(t0)')
            n = n-1
        self.produce(f'addi t0, t0, -{entity.offset}')

    def loadvr(self, v, reg):
        if str(v).isdigit():
            self.produce(f'li {reg}, {v}')
        else:
            scope, entity = self.symbolTable.searchByName(entityName=v)
            currentLevel = self.symbolTable.levelCounter-1
            targetLevel = scope.level
            entityType = type(entity).__name__
            if targetLevel == 0:
                self.produce(f'lw {reg}, -{entity.offset}(gp)')
            elif currentLevel == targetLevel and ((entityType == 'Parameter' and entity.mode == 'cv') or entityType == 'Variable' or entityType == 'TemporaryVariable'):
                self.produce(f'lw {reg}, -{entity.offset}(sp)')
            elif currentLevel == targetLevel and entityType == 'Parameter' and entity.mode == 'ref':
                self.produce(f'lw t0, -{entity.offset}(sp)')
                self.produce(f'lw {reg}, (t0)')
            elif currentLevel != targetLevel and ((entityType == 'Parameter' and entity.mode == 'cv') or entityType == 'Variable' or entityType == 'TemporaryVariable'):
                self.gnlvcode(v=v)
                self.produce(f'lw {reg}, (t0)')
            elif currentLevel != targetLevel and entityType == 'Parameter' and entity.mode == 'ref':
                self.gnlvcode(v=v)
                self.produce(f'lw t0, (t0)')
                self.produce(f'lw {reg}, (t0)')
            else:
                print('!!!loadrv error!!!')

    def storerv(self, reg, v):
        scope, entity = self.symbolTable.searchByName(entityName=v)
        targetLevel = scope.level
        currentLevel = self.symbolTable.levelCounter - 1
        entityType = type(entity).__name__
        if targetLevel == 0:
            self.produce(f'sw {reg}, -{entity.offset}(gp)')
        elif currentLevel == targetLevel and ((entityType == 'Parameter' and entity.mode == 'cv') or entityType == 'Variable' or entityType == 'TemporaryVariable'):
            self.produce(f'sw {reg}, -{entity.offset}(sp)')
        elif currentLevel == targetLevel and entityType == 'Parameter' and entity.mode == 'ref':
            self.produce(f'lw t0, -{entity.offset}(sp)')
            self.produce(f'sw {reg}, (t0)')
        elif currentLevel != targetLevel and ((entityType == 'Parameter' and entity.mode == 'cv') or entityType == 'Variable' or entityType == 'TemporaryVariable'):
            self.gnlvcode(v=v)
            self.produce(f'sw {reg}, (t0)')
        elif currentLevel != targetLevel and entityType == 'Parameter' and entity.mode == 'ref':
            self.gnlvcode(v=v)
            self.produce('lw t0, (t0)')
            self.produce(f'sw {reg}, (t0)')
        else:
            print('!!!storevr error!!!')

    def writeAsmFile(self):
        for codeLine in self.assemblyCode:
            if ':' not in codeLine:
                self.assemblyCodeFile.write('\t'+codeLine+'\n')
            else:
                self.assemblyCodeFile.write(codeLine+'\n')

    def saveAsmFile(self):
        self.assemblyCodeFile.flush()
        self.assemblyCodeFile.close()

    def showSymbolTableSnapshots(self):
        for table in self.symbolTableManager.symbolTableList:
            print(table.__str__())

    def translateToAssembly(self):
        i = 0
        while i < len(self.intermediateCode.intermediate_code):
            quadId, quad = self.intermediateCode.intermediate_code[i]
            isMain = 0

            if len(self.symbolTableManager.symbolTableList) == 1:
                isMain = 1

            self.symbolTable = self.symbolTableManager.getLatestSnapshot()
            self.produce(f'L{quadId}:')

            if quad.operator == 'begin_block':
                self.produce(f'# {quad.operator} {quad.operand1}')

                if isMain == 1:
                    frameLength = self.symbolTable.scopes[self.symbolTable.levelCounter - 1].offset
                    self.produce(f'addi sp, sp, {frameLength}')
                    self.produce('mv gp, sp')
                    self.produce(f'L0:\n\tj L{quadId} # main', 3)
                else:
                    self.produce('sw ra, (sp)')

            elif quad.operator == 'end_block':
                if isMain == 0:
                    self.produce('lw ra, (sp)')
                    self.produce('jr ra')
                self.symbolTableManager.popSnapshot()
                self.produce(f'# {quad.operator} {quad.operand1}')

            elif quad.operator in self.operatorToOperationDecoder.keys():
                operation = self.operatorToOperationDecoder.get(quad.operator)
                self.loadvr(quad.operand1, 't1')
                self.loadvr(quad.operand2, 't2')
                self.produce(f'{operation} t1, t1, t2')
                self.storerv('t1', quad.operand3)

            elif quad.operator in self.relOperatorToOperationDecoder.keys():
                relOperation = self.relOperatorToOperationDecoder.get(quad.operator)
                self.loadvr(quad.operand1, 't1')
                self.loadvr(quad.operand2, 't2')
                self.produce(f'{relOperation} t1, t2, {quad.operand3}')

            elif quad.operator == ':=':
                self.loadvr(quad.operand1, 't1')
                self.storerv('t1', quad.operand3)

            elif quad.operator == 'halt':
                self.produce('li a0, 0')
                self.produce('li a7, 93')
                self.produce('ecall')

            # PARAMETERS EXPERIMENTAL
            elif quad.operator == 'par':
                parCount = 1
                parOffsetIndex = len(self.assemblyCode)
                while quad.operator == 'par':
                    d = 12 + (parCount - 1) * 4

                    currentLevel = self.symbolTable.levelCounter - 1

                    if str(quad.operand1).isdigit():
                        self.loadvr(quad.operand1, 't0')
                        self.produce(f'sw t0, -{d}(fp)')
                    else:
                        scope, entity = self.symbolTable.searchByName(quad.operand1)
                        entityType = type(entity).__name__
                        offset = entity.offset
                        targetLevel = scope.level

                        if quad.operand2 == 'cv':
                            self.loadvr(entity.name, 't0')
                            self.produce(f'sw t0, -{d}(fp)')

                        elif quad.operand2 == 'ref':
                            if targetLevel == currentLevel:
                                if entityType == "Variable" or (entityType == "Parameter" and entity.mode == "cv"):
                                    self.produce(f'addi t0, sp, {-offset}')
                                    self.produce(f'sw t0, -{d}(fp)')
                                elif entityType == "Parameter" and entity.mode == "ref":
                                    self.produce(f'lw t0, -{offset}(sp)')
                                    self.produce(f'sw t0, -{d}(fp)')

                            elif targetLevel != currentLevel:
                                if entityType == "Variable" or (entityType == "Parameter" and entity.mode == "cv"):
                                    self.gnlvcode(v=entity.name)
                                    self.produce(f'sw t0, -{d}(fp)')
                                elif entityType == "Parameter" and entity.mode == "ref":
                                    self.gnlvcode(entity.name)
                                    self.produce('lw t0, (t0)')
                                    self.produce(f'sw t0, -{d}(fp)')

                        elif quad.operand2 == 'ret':
                            self.produce(f'addi t0, sp, -{offset}')
                            self.produce('sw t0, -8(fp)')

                    i = i + 1
                    parCount = parCount + 1
                    quadId, quad = self.intermediateCode.intermediate_code[i]
                else:
                    if quad.operator == 'call':
                        scope, entity = self.symbolTable.searchByName(quad.operand1)
                        targetLevel = scope.level
                        currentLevel = self.symbolTable.levelCounter - 1
                        #
                        self.produce(f'addi fp, sp, {entity.frameLength}', parOffsetIndex)
                        #
                        if targetLevel != currentLevel:
                            self.produce('lw t0, -4(sp)')
                            self.produce('sw t0, -4(fp)')
                        else:
                            self.produce('sw sp, -4(fp)')
                        #
                        self.produce(f'addi sp, sp, {entity.frameLength}')
                        self.produce(f'jal L{entity.startingQuad}')
                        self.produce(f'addi sp, sp, -{entity.frameLength}')

            # PARAMETERS EXPERIMENTAL
            elif quad.operator == 'call':
                scope, entity = self.symbolTable.searchByName(quad.operand1)
                targetLevel = scope.level
                currentLevel = self.symbolTable.levelCounter - 1
                #
                self.produce(f'addi fp, sp, {entity.frameLength}')
                #
                if targetLevel != currentLevel:
                    self.produce('lw t0, -4(sp)')
                    self.produce('sw t0, -4(fp)')
                else:
                    self.produce('sw sp, -4(fp)')
                #
                self.produce(f'addi sp, sp, {entity.frameLength}')
                self.produce(f'jal L{entity.startingQuad}')
                self.produce(f'addi sp, sp, -{entity.frameLength}')

            elif quad.operator == 'jump':
                self.produce(f'j {quad.operand3}')

            elif quad.operator == 'in':
                self.produce(f'li a7, 5')
                self.produce('ecall')
                self.storerv('a0', quad.operand1)

            elif quad.operator == 'out':
                self.loadvr(quad.operand1, 'a0')
                self.produce('li a7, 1')
                self.produce('ecall')
                # print new line after every print
                self.produce('la a0, str_nl')
                self.produce('li a7, 4')
                self.produce('ecall')

            elif quad.operator == 'ret':
                self.loadvr(quad.operand3, 't1')
                self.produce('lw t0, -8(sp)')
                self.produce('sw t1, (t0)')
                self.produce('lw ra, (sp)')
                self.produce('jr ra')

            else:
                print(f'!!!Unknown instruction error!!! {quad}')
            i = i + 1



class SyntaxAnalyzer:

    def __init__(self, lex, intermediateCode: IntermediateCode, symbolTable: Table, asmCode: AssemblyCode):
        self.lex = lex
        self.intermediate = intermediateCode
        self.table = symbolTable
        self.asmCode = asmCode

    def error(self, message):
        print("Syntax error," + message)
        exit(1)

    def varlist(self):
        if self.lex.get_next_token().family != "identifier":
            self.error("expected identifier at declaration, line " + self.lex.get_next_token().line_number)
        varName = self.lex.pop_next_token().recognized_string  # consume ID
        #
        self.table.addVariable(varName)
        #
        while True:
            if self.lex.get_next_token().recognized_string == ',':
                self.lex.pop_next_token()  # consume ','
                if self.lex.get_next_token().family == "identifier":
                    varName = self.lex.pop_next_token().recognized_string
                    #
                    self.table.addVariable(varName)
                    #
                else:
                    self.error("expected identifier at declaration, line " + self.lex.get_next_token().line_number)
            else:
                break
        return

    def declarations(self):
        while True:
            if self.lex.get_next_token().recognized_string == "declare":
                self.lex.pop_next_token()  # consume declare
                self.varlist()
                if self.lex.get_next_token().recognized_string == ';':
                    self.lex.pop_next_token()  # consume ';'
                else:
                    self.error("expected ';' at end of declaration, line " + self.lex.get_next_token().line_number)
            else:
                break
        return

    def optional_sign(self):
        if self.lex.get_next_token().family == "addOperator":
            S = self.lex.pop_next_token().recognized_string  # consume optional sign
            return S
        else:
            # empty
            return ""

    def actualparitem(self):
        parItem = tuple()
        if self.lex.get_next_token().recognized_string == "in":
            self.lex.pop_next_token()  # consume in
            par_name = self.expression()
            par_mode = "cv"
            parItem = (par_name, par_mode)
        elif self.lex.get_next_token().recognized_string == "inout":
            self.lex.pop_next_token()  # consume out
            if self.lex.get_next_token().family == "identifier":
                par_name = self.lex.pop_next_token().recognized_string  # consume identifier
                par_mode = "ref"
                parItem = (par_name, par_mode)
            else:
                self.error(
                    f"expected identifier after inout but instead got {self.lex.get_next_token().recognized_string} "
                    f"at line, {self.lex.get_next_token().line_number}")
        return parItem

    def actualparlist(self):
        par_list = [self.actualparitem()]
        while True:
            if self.lex.get_next_token().recognized_string == ',':
                self.lex.pop_next_token()  # consume ','
                par_list.append(self.actualparitem())
            else:
                break
        return par_list

    def idtail(self):
        F = self.lex.pop_next_token().recognized_string  # consume identifier
        if self.lex.get_next_token().recognized_string == '(':  # opening bracket for parameter list
            self.lex.pop_next_token()  # consume opening bracket
            par_list = self.actualparlist()
            if self.lex.get_next_token().recognized_string == ')':
                self.lex.pop_next_token()
                f_name = F
                F = self.intermediate.new_temp()
                self.table.addTempVariable(F)

                for par in par_list:
                    if len(par) != 0:
                        par_name, par_mode = par
                        self.intermediate.gen_quad("par", par_name, par_mode, "_")

                self.intermediate.gen_quad("par", F, "ret", "_")

                self.intermediate.gen_quad("call", f_name, "_", "_")
                return F
            else:
                self.error(f"missing ')' at end of parameter list, at line {self.lex.get_next_token().line_number}")
        else:
            # empty
            return F

    def factor(self):
        if self.lex.get_next_token().family == "number":  # INTEGER
            F = self.lex.pop_next_token().recognized_string  # consume number
            return F
        elif self.lex.get_next_token().recognized_string == '(':  # opening bracket for expression
            self.lex.pop_next_token()  # consume '('
            F = self.expression()
            if self.lex.get_next_token().recognized_string != ')':
                self.error("missing ')' at end of expression")
            self.lex.pop_next_token()  # consume ')'
            return F
        elif self.lex.get_next_token().family == "identifier":  # ID
            F = self.idtail()
            return F
        else:
            self.error(f"factor not found after term, instead got {self.lex.get_next_token().recognized_string}")

    def term(self):
        F1 = self.factor()
        while True:
            if self.lex.get_next_token().family == "multOperator":
                w = self.intermediate.new_temp()
                self.table.addTempVariable(w)
                multOperator = self.lex.pop_next_token().recognized_string  # consume * or / operator
                F2 = self.factor()
                self.intermediate.gen_quad(multOperator, F1, F2, w)
                F1 = w
            else:
                break
        E = F1
        return E

    def expression(self):
        S = self.optional_sign()
        T1 = self.term()

        #
        if S == "-":
            w = self.intermediate.new_temp()
            self.table.addTempVariable(w)
            self.intermediate.gen_quad(S, "0", T1, w)
            T1 = w
        #

        while True:
            if self.lex.get_next_token().family == "addOperator":
                w = self.intermediate.new_temp()
                self.table.addTempVariable(w)
                addOperator = self.lex.pop_next_token().recognized_string  # consume + or - operator
                T2 = self.term()
                self.intermediate.gen_quad(addOperator, T1, T2, w)
                T1 = w
            else:
                break
        E = T1
        return E

    def assign_statement(self):
        b = self.lex.pop_next_token().recognized_string  # consume identifier
        if self.lex.get_next_token().family == "assignment":
            self.lex.pop_next_token()  # consume ':=' symbol
            a = self.expression()
            self.intermediate.gen_quad(":=", a, "_", b)
        else:
            self.error(f"expected ':=' after identifier, instead got {self.lex.get_next_token().recognized_string}"
                       f" at line {self.lex.get_next_token().line_number}")

    def boolfactor(self):
        R = BoolList()
        if self.lex.get_next_token().recognized_string == "not":  # not of condition
            self.lex.pop_next_token()  # consume not
            if self.lex.get_next_token().recognized_string == '[':  # [ needed at start of condition
                self.lex.pop_next_token()  # consume [
                B = self.condition()  # call condition
                R.true = B.false
                R.false = B.true
                if self.lex.get_next_token().recognized_string == ']':  # found ']'
                    self.lex.pop_next_token()
                    return R
                else:
                    self.error(self.error("expected ']' at line " + self.lex.get_next_token().line_number))
            else:
                self.error("expected '[' at line " + self.lex.get_next_token().line_number)
        elif self.lex.get_next_token().recognized_string == '[':  # just '[' found
            self.lex.pop_next_token()  # consume '['
            B = self.condition()  # call condition
            R.true = B.true
            R.false = B.false
            if self.lex.get_next_token().recognized_string == ']':  # found ']'
                self.lex.pop_next_token()  # consume ']'
                return R
            else:
                self.error("expected ']' at line " + self.lex.get_next_token().line_number)
        else:
            E1 = self.expression()
            if self.lex.get_next_token().family == "relOperator":
                relOp = self.lex.pop_next_token().recognized_string  # consume relational operator
                E2 = self.expression()
                R.true = self.intermediate.make_list(self.intermediate.next_quad())
                self.intermediate.gen_quad(relOp, E1, E2, "_")
                R.false = self.intermediate.make_list(self.intermediate.next_quad())
                self.intermediate.gen_quad("jump", "_", "_", "_")
                return R
            else:
                self.error("expected expression at line " + self.lex.get_next_token().line_number)

    def boolterm(self):
        Q = BoolList()
        R1 = self.boolfactor()
        Q.true = R1.true
        Q.false = R1.false
        while True:
            if self.lex.get_next_token().recognized_string == "and":
                self.intermediate.backpatch(Q.true, self.intermediate.next_quad())
                self.lex.pop_next_token()
                R2 = self.boolfactor()
                Q.true = R2.true
                Q.false = self.intermediate.merge(Q.false, R2.false)
            else:
                break
        return Q

    def condition(self):
        B = BoolList()
        Q1 = self.boolterm()
        B.true = Q1.true
        B.false = Q1.false
        while True:
            if self.lex.get_next_token().recognized_string == "or":
                self.intermediate.backpatch(B.false, self.intermediate.next_quad())
                self.lex.pop_next_token()
                Q2 = self.boolterm()
                B.true = self.intermediate.merge(B.true, Q2.true)
                B.false = Q2.false
            else:
                break
        return B

    def statements(self):
        if self.lex.get_next_token().recognized_string == '{':  # bracket found
            self.lex.pop_next_token()  # consume '{'
            self.statement()
            while True:
                if self.lex.get_next_token().recognized_string == ';':
                    self.lex.pop_next_token()  # consume ';'
                    self.statement()
                else:
                    break
            if self.lex.get_next_token().recognized_string == '}':
                self.lex.pop_next_token()
            else:
                self.error("missing '}' at line " + self.lex.get_next_token().line_number)
        else:
            self.statement()
            if self.lex.get_next_token().recognized_string == ';':  # must end with ';'
                self.lex.pop_next_token()  # consume ';'
            else:
                self.error("missing ';' at line " + self.lex.get_next_token().line_number)

    def else_part(self):
        if self.lex.get_next_token().recognized_string == "else":  # if else part exists
            self.lex.pop_next_token()  # consume else
            self.statements()

    def if_statement(self):
        self.lex.pop_next_token()  # consume if
        if self.lex.get_next_token().recognized_string == '(':  # begin of condition
            self.lex.pop_next_token()  # consume '('
            B = self.condition()
            if self.lex.get_next_token().recognized_string == ')':  # end of condition
                self.lex.pop_next_token()  # consume ')'
                self.intermediate.backpatch(B.true, self.intermediate.next_quad())
                self.statements()
                ifList = self.intermediate.make_list(self.intermediate.next_quad())
                self.intermediate.gen_quad('jump', '_', '_', '_')
                self.intermediate.backpatch(B.false, self.intermediate.next_quad())
                self.else_part()
                self.intermediate.backpatch(ifList, self.intermediate.next_quad())
            else:
                self.error("expected ')' at line " + self.lex.get_next_token().line_number)
        else:
            self.error("expected '(' at line " + self.lex.get_next_token().line_number)
        return

    def while_statement(self):
        self.lex.pop_next_token()  # consume while
        if self.lex.get_next_token().recognized_string == '(':  # begin of condition
            self.lex.pop_next_token()  # consume '('
            condQuad = self.intermediate.next_quad()
            B = self.condition()
            self.intermediate.backpatch(B.true, self.intermediate.next_quad())
            if self.lex.get_next_token().recognized_string == ')':  # end of condition
                self.lex.pop_next_token()  # consume ')'
                self.statements()

                self.intermediate.gen_quad('jump', '_', '_', "L"+str(condQuad))

                self.intermediate.backpatch(B.false, self.intermediate.next_quad())
            else:
                self.error("expected ')' at line " + self.lex.get_next_token().line_number)
        else:
            self.error("expected '(' at line " + self.lex.get_next_token().line_number)

    def switchcase_statement(self):
        self.lex.pop_next_token()  # consume switchcase
        exitList = self.intermediate.empty_list()
        while True:
            if self.lex.get_next_token().recognized_string == "case":
                self.lex.pop_next_token()  # consume case
                if self.lex.get_next_token().recognized_string == '(':  # begin of condition
                    self.lex.pop_next_token()  # consume '('
                    B = self.condition()
                    self.intermediate.backpatch(B.true, self.intermediate.next_quad())
                    if self.lex.get_next_token().recognized_string == ')':  # end of condition
                        self.lex.pop_next_token()  # consume ')'
                        self.statements()
                        t = self.intermediate.make_list(self.intermediate.next_quad())

                        self.intermediate.gen_quad("jump", "_", "_", "_")

                        exitList = self.intermediate.merge(exitList, t)
                        self.intermediate.backpatch(B.false, self.intermediate.next_quad())
                    else:
                        self.error("expected ')' at line " + self.lex.get_next_token().line_number)
                else:
                    self.error("expected '(' at line " + self.lex.get_next_token().line_number)
            else:
                break
        if self.lex.get_next_token().recognized_string == "default":
            self.lex.pop_next_token()  # consume default
            self.statements()
            self.intermediate.backpatch(exitList, self.intermediate.next_quad())
        else:
            self.error("missing default statement at line " + self.lex.get_next_token().line_number)

    def forcase_statement(self):
        self.lex.pop_next_token()  # consume forcase
        firstCondQuad = self.intermediate.next_quad()
        while True:
            if self.lex.get_next_token().recognized_string == "case":
                self.lex.pop_next_token()  # consume case
                if self.lex.get_next_token().recognized_string == '(':  # begin of condition
                    self.lex.pop_next_token()  # consume '('
                    B = self.condition()
                    self.intermediate.backpatch(B.true, self.intermediate.next_quad())
                    if self.lex.get_next_token().recognized_string == ')':  # end of condition
                        self.lex.pop_next_token()  # consume ')'
                        self.statements()

                        self.intermediate.gen_quad("jump", "_", "_", "L"+str(firstCondQuad))

                        self.intermediate.backpatch(B.false, self.intermediate.next_quad())
                    else:
                        self.error("expected ')' at line " + self.lex.get_next_token().line_number)
                else:
                    self.error("expected '(' at line " + self.lex.get_next_token().line_number)
            else:
                break
        if self.lex.get_next_token().recognized_string == "default":
            self.lex.pop_next_token()  # consume default
            self.statements()
        else:
            self.error("missing default statement at line " + self.lex.get_next_token().line_number)

    def incase_statement(self):
        self.lex.pop_next_token()  # consume incase
        flag = self.intermediate.new_temp()
        self.table.addTempVariable(flag)
        firstCondQuad = self.intermediate.next_quad()
        self.intermediate.gen_quad(":=", "0", "_", flag)
        while True:
            if self.lex.get_next_token().recognized_string == "case":
                self.lex.pop_next_token()  # consume case
                if self.lex.get_next_token().recognized_string == '(':  # begin of condition
                    self.lex.pop_next_token()  # consume '('
                    B = self.condition()
                    self.intermediate.backpatch(B.true, self.intermediate.next_quad())
                    if self.lex.get_next_token().recognized_string == ')':  # end of condition
                        self.lex.pop_next_token()  # consume ')'
                        self.statements()

                        self.intermediate.gen_quad(":=", "1", "_", flag)
                        self.intermediate.backpatch(B.false, self.intermediate.next_quad())
                    else:
                        self.error("expected ')' at line " + self.lex.get_next_token().line_number)
                else:
                    self.error("expected '(' at line " + self.lex.get_next_token().line_number)
            else:

                self.intermediate.gen_quad("=", "1", flag, "L"+str(firstCondQuad))
                break

    def return_statement(self):
        self.lex.pop_next_token()  # consume return
        if self.lex.get_next_token().recognized_string == '(':  # opening '('
            self.lex.pop_next_token()  # consume '('
            E = self.expression()
            self.intermediate.gen_quad("ret", "_", "_", E)
            if self.lex.get_next_token().recognized_string == ')':  # closing ')'
                self.lex.pop_next_token()
            else:
                self.error("missing ')' at return statement, line " + self.lex.get_next_token().line_number)
        else:
            self.error("missing '(' at return statement, line " + self.lex.get_next_token().line_number)

    def print_statement(self):
        self.lex.pop_next_token()  # consume print
        if self.lex.get_next_token().recognized_string == '(':  # opening '('
            self.lex.pop_next_token()  # consume '('
            E = self.expression()
            if self.lex.get_next_token().recognized_string == ')':  # closing ')'
                self.lex.pop_next_token()  # consume ')'
                #
                self.intermediate.gen_quad("out", E, "_", "_")
                #
            else:
                self.error("missing ')' at print statement, line " + self.lex.get_next_token().line_number)
        else:
            self.error("missing '(' at print statement, line " + self.lex.get_next_token().line_number)

    def input_statement(self):
        self.lex.pop_next_token()  # consume input
        if self.lex.get_next_token().recognized_string == '(':  # opening '('
            self.lex.pop_next_token()  # consume '('
            if self.lex.get_next_token().family == "identifier":
                I = self.lex.pop_next_token().recognized_string  # consume identifier
                if self.lex.get_next_token().recognized_string == ')':  # closing ')'
                    self.lex.pop_next_token()  # consume ')'
                    #
                    self.intermediate.gen_quad("in", I, "_", "_")
                    #
                else:
                    self.error("missing ')' at input statement, line " + self.lex.get_next_token().line_number)
            else:
                self.error("expected identifier at input statement, line " + self.lex.get_next_token().line_number)
        else:
            self.error("missing '(' at input statement, line " + self.lex.get_next_token().line_number)

    def call_statement(self):
        self.lex.pop_next_token()  # consume call
        if self.lex.get_next_token().family == "identifier":
            process_name = self.lex.pop_next_token().recognized_string  # consume identifier
            if self.lex.get_next_token().recognized_string == '(':
                self.lex.pop_next_token()
                par_list = self.actualparlist()
                if self.lex.get_next_token().recognized_string == ')':
                    self.lex.pop_next_token()

                    for par in par_list:
                        if len(par) != 0:
                            parName, parMode = par
                            self.intermediate.gen_quad("par", parName, parMode, "_")

                    self.intermediate.gen_quad("call", process_name, "_", "_")
                else:
                    self.error("missing ')' at call statement, line " + self.lex.get_next_token().line_number)
            else:
                self.error("missing '(' at call statement, line " + self.lex.get_next_token().line_number)
        else:
            self.error("expected identifier at call statement, line " + self.lex.get_next_token().line_number)

    def statement(self):
        if self.lex.get_next_token().family == "identifier":
            self.assign_statement()
        elif self.lex.get_next_token().recognized_string == "if":
            self.if_statement()
        elif self.lex.get_next_token().recognized_string == "while":
            self.while_statement()
        elif self.lex.get_next_token().recognized_string == "switchcase":
            self.switchcase_statement()
        elif self.lex.get_next_token().recognized_string == "forcase":
            self.forcase_statement()
        elif self.lex.get_next_token().recognized_string == "incase":
            self.incase_statement()
        elif self.lex.get_next_token().recognized_string == "return":
            self.return_statement()
        elif self.lex.get_next_token().recognized_string == "print":
            self.print_statement()
        elif self.lex.get_next_token().recognized_string == "input":
            self.input_statement()
        elif self.lex.get_next_token().recognized_string == "call":
            self.call_statement()
        else:
            # empty
            return

    def block_statements(self):
        self.statement()
        while True:
            if self.lex.get_next_token().recognized_string == ';':
                self.lex.pop_next_token()  # consume ';'
                self.statement()  # get next statement
            else:
                break

    def formalparitem(self):
        parItem = tuple()
        if self.lex.get_next_token().recognized_string == "in":
            self.lex.pop_next_token()  # consume in/inout
            parMode = "cv"
            if self.lex.get_next_token().family == "identifier":
                parName = self.lex.pop_next_token().recognized_string  # consume identifier
                parItem = (parName, parMode)
            else:
                self.error(
                    f"expected identifier after in/inout but instead got {self.lex.get_next_token().recognized_string} "
                    f"at line, {self.lex.get_next_token().line_number}")
        elif self.lex.get_next_token().recognized_string == "inout":
            self.lex.pop_next_token()
            parMode = "ref"
            if self.lex.get_next_token().family == "identifier":
                parName = self.lex.pop_next_token().recognized_string  # consume identifier
                parItem = (parName, parMode)
            else:
                self.error(
                    f"expected identifier after in/inout but instead got {self.lex.get_next_token().recognized_string} "
                    f"at line, {self.lex.get_next_token().line_number}")
        return parItem

    def formalparlist(self):
        parList = []
        parItem = self.formalparitem()
        parList.append(parItem)
        while True:
            if self.lex.get_next_token().recognized_string == ',':
                self.lex.pop_next_token()
                parItem = self.formalparitem()
                parList.append(parItem)
            else:
                break
        return parList

    def subprogram(self):
        subProgramType = self.lex.get_next_token().recognized_string
        if subProgramType == "procedure":
            self.lex.pop_next_token()
            if self.lex.get_next_token().family == "identifier":
                block_name = self.lex.get_next_token().recognized_string
                #
                self.table.addProcedure(block_name)
                #
                self.lex.pop_next_token()
                if self.lex.get_next_token().recognized_string == '(':
                    self.lex.pop_next_token()
                    parList = self.formalparlist()
                    for par in parList:
                        if len(par) != 0:
                            parName, parMode = par
                            self.table.addFormalParameter(parName, parMode)
                    if self.lex.get_next_token().recognized_string == ')':
                        self.lex.pop_next_token()

                        self.table.addLevel()
                        for par in parList:
                            if len(par) != 0:
                                parName, parMode = par
                                self.table.addParameter(parName, parMode)

                        self.block(block_name)
                        return 1
                    else:
                        self.error("missing ')' at subprogram at line " + self.lex.get_next_token().line_number)
                else:
                    self.error("missing '(' at subprogram at line " + self.lex.get_next_token().line_number)
            else:
                self.error("missing identifier at subprogram at line " + self.lex.get_next_token().line_number)
        elif subProgramType == "function":
            self.lex.pop_next_token()
            if self.lex.get_next_token().family == "identifier":
                block_name = self.lex.get_next_token().recognized_string
                #
                self.table.addFunction(block_name)
                #
                self.lex.pop_next_token()
                if self.lex.get_next_token().recognized_string == '(':
                    self.lex.pop_next_token()
                    parList = self.formalparlist()
                    for par in parList:
                        if len(par) != 0:
                            parName, parMode = par
                            self.table.addFormalParameter(parName, parMode)
                    if self.lex.get_next_token().recognized_string == ')':
                        self.lex.pop_next_token()
                        #
                        self.table.addLevel()
                        for par in parList:
                            if len(par) != 0:
                                parName, parMode = par
                                self.table.addParameter(parName, parMode)
                        #
                        self.block(block_name)
                        return 1
                    else:
                        self.error("missing ')' at subprogram at line " + self.lex.get_next_token().line_number)
                else:
                    self.error("missing '(' at subprogram at line " + self.lex.get_next_token().line_number)
            else:
                self.error("missing identifier at subprogram at line " + self.lex.get_next_token().line_number)
        else:
            return -1

    def subprograms(self):
        while True:
            if self.subprogram() == -1:
                break

    def block(self, block_name, main=0):
        if self.lex.pop_next_token().recognized_string != '{':
            self.error("missing '{', instead found " + self.lex.get_next_token().recognized_string)
        self.declarations()
        self.subprograms()
        startingQuad = self.intermediate.next_quad()
        self.intermediate.gen_quad("begin_block", block_name, "_", "_")

        self.block_statements()

        frameLength = self.table.scopes[self.table.levelCounter - 1].offset

        if main == 1:
            self.intermediate.gen_quad("halt", "_", "_", "_")

        if main == 0:
            self.table.updateStartingQuad(startingQuad)
            self.table.updateFrameLength(frameLength)
        #
        self.intermediate.gen_quad("end_block", block_name, "_", "_")
        #
        self.table.write_symb_file()

        tableCopy = Table(self.table.scopes, self.table.levelCounter)
        self.table.popLevel()
        self.asmCode.symbolTableManager.addSnapshot(tableCopy)
        #

        if self.lex.get_next_token().recognized_string != '}':
            self.error("missing '}', instead found " + self.lex.get_next_token().recognized_string +
                       " at line " + self.lex.get_next_token().line_number)
        self.lex.pop_next_token()

        return startingQuad

    def perform_syntax_analysis(self):
        if self.lex.pop_next_token().recognized_string != "program":
            self.error("'program' program keyword not found")
        block_name = self.lex.get_next_token().recognized_string
        self.table.addLevel()
        if self.lex.pop_next_token().family != "identifier":
            self.error("id program id not found")

        self.block(block_name, 1)

        if self.lex.pop_next_token().recognized_string != ".":
            self.error("'.' delimiter not found")
        if self.lex.pop_next_token().recognized_string != "eof":
            self.error("text after delimiter '.' error")
        #print("Syntax analysis successful.")
        return self.intermediate, self.table



class EqualInC:

    def __init__(self, intermediate_code):
        self.intermediate_code = intermediate_code
        self.equal_code_file = open('a.c', 'w')

    def translateToC(self):
        #print('Translating .ci file to .c...')
        self.equal_code_file.write('''#include <stdio.h>\nint main()\n{\n''')
        variables = []
        for quad_id, quad in self.intermediate_code:
            if quad.operator == ':=':
                if quad.operand3 not in variables:
                    self.equal_code_file.write(f'\tL{quad_id}: ;\n int {quad.operand3} = {quad.operand1};\n')
                    variables.append(quad.operand3)
                else:
                    self.equal_code_file.write(f'\tL{quad_id}: {quad.operand3} = {quad.operand1};\n')
            elif quad.operator == 'begin_block' or quad.operator == 'end_block':
                self.equal_code_file.write(f'\tL{quad_id}: ; /* {quad.operand1} */\n')
            elif quad.operator == 'halt':
                self.equal_code_file.write(f'\tL{quad_id}: return 0;\n')
            elif quad.operator in ['+', '-', '*', '/']:
                self.equal_code_file.write(f'\tL{quad_id}: ;\n\tint {quad.operand3} = {quad.operand1} {quad.operator} {quad.operand2};\n')
            elif quad.operator == 'in':
                if quad.operand1 not in variables:
                    self.equal_code_file.write(f'\tL{quad_id}: ;\n int {quad.operand1};\n')
                    variables.append(quad.operand1)
                    self.equal_code_file.write(f'\tscanf(\"%d\", &{quad.operand1});\n')
                else:
                    self.equal_code_file.write(f'\tL{quad_id}: scanf(\"%d\", &{quad.operand1});\n')
            elif quad.operator == 'out':
                self.equal_code_file.write(f'\tL{quad_id}: printf(\"%d\\n\", {quad.operand1});\n')
            elif quad.operator == 'jump':
                self.equal_code_file.write(f'\tL{quad_id}: goto {quad.operand3};\n')
            elif quad.operator in ['<', '>', '=', '<=', '>=', '<>']:
                operator = quad.operator
                if operator == '<>':
                    operator = '!='
                elif operator == '=':
                    operator = '=='
                self.equal_code_file.write(f'\tL{quad_id}: if({quad.operand1} {operator} {quad.operand2})')
                self.equal_code_file.write('\t{ ')
                self.equal_code_file.write(f'goto {quad.operand3};')
                self.equal_code_file.write(' }\n')

        self.equal_code_file.write('}')
        self.equal_code_file.close()
        #print('Done translating.')



class Main:
    print(
        '''
               _____      _                 _      
              / ____|    (_)               | |     
             | |   ______ _ _ __ ___  _ __ | | ___ 
             | |  |______| | '_ ` _ \| '_ \| |/ _ \\
             | |____     | | | | | | | |_) | |  __/
              \_____|    |_|_| |_| |_| .__/|_|\___|
                                     | |           
                                     |_|                                         
            ''')

    file_name = sys.argv[1:][0]

    if not file_name.endswith('.ci'):
        print('No cimple file was given')
        exit(1)
    print(file_name)

    lexicalAnalyzer = LexicalAnalyzer(file_name=file_name)
    lexicalAnalyzer.perform_lexical_analysis()

    symbolTableManager = TableManager()

    symbolTable = Table()
    symbolTable.open_symb_file()
    intermediateCode = IntermediateCode()

    asmCode = AssemblyCode(symbolTable=symbolTable, intermediateCode=intermediateCode, symbolTableManager=symbolTableManager)
    syntaxAnalyzer = SyntaxAnalyzer(lex=lexicalAnalyzer,
                                    intermediateCode=intermediateCode,
                                    symbolTable=symbolTable,
                                    asmCode=asmCode)
    syntaxAnalyzer.perform_syntax_analysis()
    intermediateCode.write_int_file()
    intermediateCode.save_int_file()
    symbolTable.save_symb_file()
    equalInC = EqualInC(intermediateCode.intermediate_code)
    equalInC.translateToC()
    asmCode.translateToAssembly()

    asmCode.writeAsmFile()
    asmCode.saveAsmFile()
    print('Done compiling.')
    # the program was 1499 lines so I added this to make it 1500