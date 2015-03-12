int fact(int n) {
    for (int res = n; n > 1; n--) {
        res *= n;
    }
    return res;
}
