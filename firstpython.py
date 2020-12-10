from math import pow
from math import pi

aristacubo=10
rcono=3
epsilon=1
iteraciones=int(aristacubo/epsilon)
volumen=0
controlh=0
for i in range(1,iteraciones+1):
  controlh=controlh+epsilon
  r=3*(aristacubo-controlh)/aristacubo
  area=pow(aristacubo,2)-(pi*pow(r,2))
  seccion=area*epsilon
  volumen=volumen+seccion
print("el volumen del generado por la diferencia entre el cubo y el cono es: ")
print(volumen)
