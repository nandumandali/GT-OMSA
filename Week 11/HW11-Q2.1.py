

import pandas as pd

from pulp import *

data = pd.read_excel("diet.xls")

#print(data)

data=data[0:64]

data = data.values.tolist()

foods = [x[0] for x in data]

cost = dict([(x[0], float(x[1])) for x in data])

calories = dict([(x[0], float(x[3])) for x in data])

totalFat = dict([(x[0], float(x[5])) for x in data])

cholesterol = dict([(x[0], float(x[4])) for x in data])

sodium = dict([(x[0], float(x[6])) for x in data])

carbohydrates = dict([(x[0], float(x[7])) for x in data])

dietaryFiber = dict([(x[0], float(x[8])) for x in data])

protein = dict([(x[0], float(x[9])) for x in data])

vitA = dict([(x[0], float(x[10])) for x in data])

vitC = dict([(x[0], float(x[11])) for x in data])

calcium = dict([(x[0], float(x[12])) for x in data])

iron = dict([(x[0], float(x[13])) for x in data])


tut = LpProblem('PuLPTutorial',LpMinimize)

foodVars = LpVariable.dicts("Foods", foods, 0)

chosenVars = LpVariable.dicts("Chosen", foods,0,1,'Binary')

tut += lpSum([cost[f]*foodVars[f] for f in foods]), 'Total Cost'

tut += lpSum([calories[f] * foodVars[f] for f in foods]) >= 1500, 'min Calories'
tut += lpSum([calories[f] * foodVars[f] for f in foods]) <= 2500, 'max Calories'

tut += lpSum([totalFat[f] * foodVars[f] for f in foods]) >= 20, 'min Fat'
tut += lpSum([totalFat[f] * foodVars[f] for f in foods]) <= 70, 'max Fat'

tut += lpSum([cholesterol[f] * foodVars[f] for f in foods]) >= 30, 'min Cholesterol'
tut += lpSum([cholesterol[f] * foodVars[f] for f in foods]) <= 240, 'max Cholesterol'

tut += lpSum([sodium[f] * foodVars[f] for f in foods]) >= 800, 'min Sodium'
tut += lpSum([sodium[f] * foodVars[f] for f in foods]) <= 2000, 'max Sodium'

tut += lpSum([carbohydrates[f] * foodVars[f] for f in foods]) >= 130, 'min Carbohydrates'
tut += lpSum([carbohydrates[f] * foodVars[f] for f in foods]) <= 450, 'max Carbohydrates'

tut += lpSum([dietaryFiber[f] * foodVars[f] for f in foods]) >= 125, 'min Dietary Fiber'
tut += lpSum([dietaryFiber[f] * foodVars[f] for f in foods]) <= 250, 'max Dietary Fiber'

tut += lpSum([protein[f] * foodVars[f] for f in foods]) >= 60, 'min Protein'
tut += lpSum([protein[f] * foodVars[f] for f in foods]) <= 100, 'max Protein'

tut += lpSum([vitA[f] * foodVars[f] for f in foods]) >= 1000, 'min vitamin A'
tut += lpSum([vitA[f] * foodVars[f] for f in foods]) <= 10000, 'max vitamin A'

tut += lpSum([vitC[f] * foodVars[f] for f in foods]) >= 400, 'min vitamin C'
tut += lpSum([vitC[f] * foodVars[f] for f in foods]) <= 5000, 'max vitamin C'

tut += lpSum([calcium[f] * foodVars[f] for f in foods]) >= 700, 'min Calcium'
tut += lpSum([calcium[f] * foodVars[f] for f in foods]) <= 1500, 'max Calcium'

tut += lpSum([iron[f] * foodVars[f] for f in foods]) >= 10, 'min Iron'
tut += lpSum([iron[f] * foodVars[f] for f in foods]) <= 40, 'max Iron'

tut.writeLP("DietProblem.lp")

tut.solve()

print("Status:", LpStatus[tut.status])

print()
print("-------------The solution to the diet problem is--------------")
for v in tut.variables():
	if v.varValue > 0 :
		print(str(v.varValue)+ " units of "+str(v).replace('Foods_',''))

print()		
print("Total cost of food = $%.2f" % value(tut.objective))



