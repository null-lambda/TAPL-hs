/* Examples for testing */

x/;
x;

lambda x. x;
(lambda x. x) (lambda x. x x);


(\x.x) ((\x.x) (\z.(\x.x)z));
(\x.\y.\z.x z (y z)) (\x.\y.x) (\x.\y.x);

(\m n f.m (n f));
(\m n f.m (n f)) (\f.\x.f (f x)) (\f.\x.f (f (f x)));
(\b.\e. e b) (\f.\x.f (f x)) (\f.\x.f (f (f x)));

/* infinite */
(\a. a a) (\a. a a); 

/* works only on call-by-name strategy */
(\x y. y) ((\a. a a) (\a. a a));

(\x y. y) ((\a. a) (\b. b b)); 