a = csvread('berry.csv');
b = csvread('keylime.csv');
c = csvread('orange.csv');
d = csvread('lemon.csv');

appendAll = [a;b;c;d;];

A = appendAll(randperm(size(appendAll,1)),:);

csvwrite('shuffledYogurt.csv', A);