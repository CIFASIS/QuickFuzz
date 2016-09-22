# Generating better random source code with QuickFuzz
(by Franco Costantini)

QuickFuzz can generate random code from various programming languages. Obviously, most of the time this code won't be syntactically or semantically valid. One of the things random code can't account for is variable coherence, i.e., when we use a variable, it must be defined (or declared) before. 
We can see this with this Python example:  

```python
hiy = vu  
jdu = ag  
rpa = kk
zcx = 2
vp = f  
np = 2.1850528035871353  
b = 1 >> 0  
zau = a  
meg = -18.39505230244595 == p  
ize = le  
no = -2  
```

My project was about partially solving this problem, and automating the solution. Our target languages were Python, JavaScript, Lua and Bash, but (hopefully) it should be easy to apply to others.  

## Main work

### Summary

- Definition of Fixable class  
- Required information  
- Automatic instance derivation
- Functions needed

### Description

We define a class (Fixable) that will take some language construction and will fix it in a way that adds (some) variable coherence.

```haskell
data StV a = StV {vars :: [a]} deriving Show  

type VState a b = StateT (StV a) Gen b  

class Fixable a b where  
  fix :: b -> VState a b  
```

The state is simply a list of variable identifiers, and we use a monad transformer to wrap the state around Gen. Basically we will be handling the state transformer, and at the end of the fix process, we must evaluate the state.  

We derive instances for this class using Template Haskell. The process for this is simple: we need to specify the type representing identifiers (could be simply String or a wrapper type), a list of constructors which will be variables "calls" (something like Var with an identifier), another list of constructors representing assignments (like a direct assignment or a for) and finally the type for which we want to derive the instance, let's call it T.  

When we have all this, we can extract some information from these types to form the skeleton of our instance (like class restrictions on the type parameters). Then, the interesting bit is building the body of the fix function. To do this, we start travelling through the different constructors of T, and consider 3 different situations (say C is the current constructor of T being analyzed):  

- if C is one of the constructors representing variables, we must make sure its identifier exists in the current state. If this is not the case, we have 2 situations: the state is empty, in which case we change the variable expression for a different one (it could be a constant, or simply an arbitrary expression); or we have some ids in the state, and then we take one of them randomly to replace the current one.  
- if C is an assignment constructor, we must apply fix to the expression(s) that will be assigned, and then we add the assignment id to the state (this has some flaws, more info below).  
- if C is not in either of the lists, we just apply fix to its parameters.  

We also need to define some functions to make it work. These functions are somewhat difficult to derive automatically because sometimes the specified constructors are not so straightforward (actually pop and push are pretty easy):  

- getVId: extracts the identifier from a Variable expression.  
- getAId: extracts the identifier from an Assignment expression.
- printST: prints the state (not really necessary, but useful for debugging).  
- popId: deletes an identifier from the state.  
- pushId: adds an identifier to the state.  
- genCons: the expression to put in when the state is empty.  
- genVar: generates a Variable expression using an identifier from the state.  

Using Megadeth, we can get the instance derivation for an entire AST using just one line, since Megadeth can resolve type dependencies and will produce the instance for every type.  

```haskell
$(devFixLang ''Ident ['Var] ['Assign, 'For] ''Module)
```

After this, we must apply the fix somewhere. Currently this is manually done inside the Arbitrary instance for the top level type in the AST, using evalStateT to get rid of the state:  

```haskell
instance Arbitrary T where  
    arbitrary = do a <- sized go  
                   evalStateT (fix a) (initV :: StV Id)  
                where go n = ...  
```

As an example, the Lua module with variable coherence can be found [here](https://github.com/CIFASIS/QuickFuzz/blob/master/src/Lua.hs).  

## Results

We can test this with our previous Python example. After applying the fix, we get the following:

```python
hiy = 3  
jdu = hiy  
rpa = jdu  
zcz = 2  
vp = zcz  
np = 2.1850528035871353   
b = 1 >> 0  
zau = jdu  
meg = -18.39505230244595 == zcz  
ize = vp  
no = -2  
```

We can see that now we reuse previously defined variables in new assignments.  

### Testing

We ran QuickFuzz over different implementations of Bash, Python, Lua and JavaScript. So far we have found (after deduplicating the bugs using backtrace):

- [A rediscovered bug in the current version of Lua](https://www.lua.org/bugs.html#5.3.3-1) (there's a patch available).  
- [A case where Python compilation takes a very long time (sometimes forever)](http://bugs.python.org/issue27695).
- Aprox. 20 bugs in bash (waiting for feedback).
- Aprox. 10 bugs in Busybox's implementation of bash (ash) (waiting for feedback).

## Limitations

- We mentioned that the user has to define some auxiliary functions to make the derivation work. These functions are pretty easy to understand so it's (or should be) easy to do it, but it's somewhat annoying.  
- When we have assignments, there are two different situations to keep in mind: you can use the new variable in the "body" of the assignments (e.g. a for) or you can't (e.g. a normal assignment). Currently there is no way to distinguish these 2 cases, and every case is treated like it's of the second class. For example:  

```python
for i in 1:
    j = i
```

And if we "fix" it:  

```python
for i in 1:
    j = 4
```

This could be easily fixed by separating assignments into two lists (or providing more information), but we decided to leave it as it is for now.  

## Future work

- As said in the previous section, it would be desirable to automatically get the necessary functions instead of asking for the user to provide them, at least if it's easy to produce them.  
- Providing more information can lead to better results. An example could be adding function parameters to our state.
