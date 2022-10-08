#!bin\python3


def prime(n: int) -> bool:
    if n < 2: return False
    if n == 2: return True
    return and_lst_rec(list(map (lambda x : False if n % x == 0 else True, [x for x in range(2,n)])))
    
    

def and_lst_rec(lst: list[bool]) -> bool:
    if lst == []:
        return True
    if lst[0] == True:
        return and_lst_rec(lst[1::])
    if lst[0] == False:
        return False



def main():
    for i in range(2,10):
        print(f' {i} is a prime True or False -> {prime(i)}')

if __name__ == '__main__':
    main()
