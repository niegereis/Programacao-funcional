quociente' num1 num2 = quociente num1 num2 1 

quociente num1 num2 div
 | num1 < num2 =  (div - 1)
 | num1 == num2 = div
 | otherwise = quociente (num1-num2) num2 (div+1)

resto' num1 num2 = resto num1 num2 1 

resto num1 num2 div = resto
  where 
    resto =  num1 - ((quociente num1 num2 div) * num2) 