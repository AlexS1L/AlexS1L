m = int(input('Digite o primeiro n??mero: '))
n = int(input('Digite o segundo n??mero: '))
q = int(input(f'Desejas calcular quantos m??ltiplo de {m} e de {n}? '))

soma1 = soma2 = 0
mult1 = list()
mult2 = list()
while len(mult1) < q and len(mult2) < q:
    soma1 += m
    soma2 += n
    mult1.append(soma1)
    mult2.append(soma2)

print(f'Os {q} primeiros m??ltiplos de {m} s??o: {mult1}')
print(f'Os {q} primeiros m??ltiplos de {n} s??o: {mult2}')
#############
def main():
    '''
    Programa que l?? tr??s n??meros inteiros positivos n, i e j 
    e imprime os n primeiros n??meros inteiros maiores o iguais
    a zero que s??o multiplos de i ou de j.
    '''

    print("C??lculo dos n primeros multiplos de i ou j\n")

    # leitura dos dados
    n = int(input("Digite n: "))
    i = int(input("Digite i: "))
    j = int(input("Digite j: "))
    
    
    # mult ?? o candidato a multiplo de i ou j
    mult = 0

    # k ?? o contador de m??ltiplos impressos
    k    = 0
    while k < n:
        if mult%i == 0 or mult%j == 0:
            # mult ?? multiplo de i ou j
            print(mult)
            k = k + 1;
         
        # vamos para o pr??ximo candidato   
        mult = mult + 1;	

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
main()
