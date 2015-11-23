#include<stdio.h>
#include <stdlib.h>

int produced=0;

void prod(int a[5])
{
        int i;
        int produced;
        for(i=0;i<5;i++)
        {
                a[i]=(int)rand();
                printf("Produced %d\n",++produced);
        }
}

void cons(int a[5])
{
        int i;
        int count = 4;
        int consumed = 0;
        a:prod(a);
        for(i=4;i>=0;i--)
        {
                printf("Consumed %d\n",++consumed);
        }
        count--;
        if(count)
                goto a;
}

void main()
{
                int a[5];
                cons(a);
}
