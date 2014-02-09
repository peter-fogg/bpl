/* A /*program*/ to compute factorials. */

int fact( int n) {
  if(n<=1)
    return   1;
    string s = "foo";
  else
    return n*fact(n-1);
}

void main(void) {
  int x;
  x = 1;
  while(x <= 10) {
    write(x);
    write(fact(x));
    writeln();
    x = x + 1;
  }
}
