def multiadder(n, current_arg = 0):
    if n == 0:
        return current_arg
    def addn(next_arg):
        return multiadder(n-1, current_arg + next_arg)
    return addn

add = multiadder(5)
print(add(6)(4)(7)(2)(1))