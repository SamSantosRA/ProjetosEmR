#### pnorm() = F(z)
#### qnorm() = z
#### dnorm() = f(z)
#### f(z) - z * (1-F(z)) = I(z)



### Modelo - Atraso no Atendimento em Caso de Falta  ###


Dm =    as.numeric(readline(prompt = "Digite Dm   "))
sigma = as.numeric(readline(prompt = "Digite desv. pad.   "))
cp =    as.numeric(readline(prompt = "Digite cp    "))
ce =    as.numeric(readline(prompt = "Digite ce   "))
cf =    as.numeric(readline(prompt = "Digite cf   "))
LT =    as.numeric(readline(prompt = "Digite LT   "))
Am = 0
x  =   as.numeric(readline(prompt = "Digite o número de interações que serão feitas   "))

###

Dm_L = Dm * LT;

Lead_Time_Probabilistico = readline(prompt = "O Lead-Time é Probabilístico?? (sim ou nao)   ")
if (Lead_Time_Probabilistico == "sim"){
  sigma_LT = as.numeric(readline(prompt = "Digite o desv. pad. do LT   "))
  sigma_L = sqrt(LT * sigma^2 + Dm^2 * sigma_LT^2); print("LT Probabilístico")
  
}else{
  sigma_L = sqrt(LT) * sigma
}



# sigma_L = sigma /sqrt(5)  ### Acho que n é igual a 30/ LT (6 dias, no caso)

###

for (n in 1:x) {          ### É possível mudar o contador aqui
  Q = sqrt(2*Dm*(cp+cf*Am)/ce);
  F_z = (cf * Dm/ Q - ce/ 2) / (cf * Dm/ Q + ce/ 2)
  z = qnorm(F_z, mean = 0, sd = 1)
  s = Dm_L + z * sigma_L;
  f_z = dnorm(z, mean = 0, sd = 1);
  Integral_Perda = f_z - z * (1-F_z);
  Am = sigma_L * Integral_Perda;
  
  
  ###
  
  cat("\n\n Estes são os valores obtidos na interação de número", n)
  cat("\n Valor de Q = ", Q)
  cat("\n Valor de F(z) = ", F_z)
  cat("\n Valor de z = ", z)
  cat("\n Valor de s = ", s)
  cat("\n Valor de I(z) = ", Integral_Perda)
  cat("\n Valor de Am = ", Am)
  
  
}


###

Custo_Pedido = cp * Dm/ Q;
Custo_Estocagem = ce * ((Q - Am)/2 + s - Dm_L + Am);
Custo_Falta = cf * Am * Dm/ Q

Custo_Total = Custo_Estocagem + Custo_Pedido + Custo_Falta;

PCSF = F_z;
PDSF = (Q - Am) / Q;

###

cat("\n\n OS VALORES FINAIS SÃO    ")
cat("\n Valor de Q = ", Q)
cat("\n Valor de s = ", s)
cat("\n Valor de Am = ", Am)
cat("\n Valor de PCSF = ", PCSF)
cat("\n Valor de PDSF = ", PDSF)
cat("\n Valor do CTP = ", Custo_Total)





