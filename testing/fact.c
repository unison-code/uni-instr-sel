int fact(int n) {
    int res = n;
    for (; n > 1; n--) {
        res *= n;
    }
    return res;
}
