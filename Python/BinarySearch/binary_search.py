def binary_search(a, x, min, max):

    mid = (min + max) // 2
# found
    if a[mid] == x:
        return mid
# not found
    if min == max:
        return -1
# keep searching
    if a[mid] > x:
        max = mid - 1
    else:
        min = mid + 1

    return binary_search(a, x, min, max)


a = [1, 3, 7]
x = 7
min = 0
max = len(a) - 1
print( binary_search(a, x, min, max) )