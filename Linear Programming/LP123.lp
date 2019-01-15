/*objective function*/
min: 12 q1 + 14 q2 + 16 q3 +18 q4 + 2 s1 + 2 s2 + 2 s3 + 2 s4;

/*constraints*/
q1 <= 400;
q2 <= 400;
q3 <= 300;
q4 <= 300;

q1 - s1 = 100;
s1 + q2 - s2 = 200;
s2 + q3 - s3 = 150;
s3 + q4 - s4 = 400;
