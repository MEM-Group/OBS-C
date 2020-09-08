
#allocate(4,1,2,3,4);
#allocStack(0);
#write(#stack(0),#blk(0));
#allocStack(0);
#write(#stack(1),#ref(#stack(0)));
#write(#field(#stack(0),3), 5);
#read(#deref(#stack(1)))


