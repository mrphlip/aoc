#include <stdio.h>

int myinput[9] = {1,2,3,4,5,6,7,8,9};

struct node {
	int val;
	struct node *prev, *next;
	node(int val) : val(val), prev(NULL), next(NULL) {}
};

void playgame(int* inbuf, int inbuflen, int* outbuf, int outbuflen, int len, int iters) {
	node **lookup = new node*[len];
	node *head = new node(inbuf[0]);
	lookup[inbuf[0]] = head;
	node *p = head;
	for (int i = 1; i < inbuflen; i++) {
		node *n = new node(inbuf[i]);
		n->prev = p;
		p->next = n;
		lookup[inbuf[i]] = n;
		p = n;
	}
	for (int i = inbuflen; i < len; i++) {
		node *n = new node(i);
		n->prev = p;
		p->next = n;
		lookup[i] = n;
		p = n;
	}
	p->next = head;
	head->prev = p;

	//printf("%d,",head->val+1);for(p=head->next;p!=head;p=p->next)printf("%d,",p->val+1);printf("\n");
	for (int i = 0; i < iters; i++) 	{
		node *removed1 = head->next;
		node *removed2 = removed1->next;
		node *removed3 = removed2->next;
		node *removedafter = removed3->next;
		removedafter->prev = head;
		head->next = removedafter;

		int target = head->val;
		do {
			target = (target + len - 1) % len;
		} while (target == removed1->val || target == removed2->val || target == removed3->val);

		p = lookup[target];
		p->next->prev = removed3;
		removed3->next = p->next;
		p->next = removed1;
		removed1->prev = p;

		head = head->next;
		//if (i % 1000 == 0) printf("%d\n", i);
		//printf("%d,",head->val+1);for(p=head->next;p!=head;p=p->next)printf("%d,",p->val+1);printf("\n");
	}

	for (p=head->next; p!=head; p=p->next)
		if (p->val == 0)
			break;
	for (int i = 0; i < outbuflen; i++) {
		p = p->next;
		outbuf[i] = p->val;
	}

	head->prev->next = NULL;
	for (p = head; p; p = p->next)
		delete p;
	delete[] lookup;
}

int main() {
	int testdata[9] = {2,7,8,0,1,4,3,5,6};
	int outbuf[8];

	playgame(testdata, 9, outbuf, 8, 9, 10);
	for (int i = 0; i < 8; i++)
		printf("%d", outbuf[i] + 1);
	printf("\n");
	playgame(testdata, 9, outbuf, 8, 9, 100);
	for (int i = 0; i < 8; i++)
		printf("%d", outbuf[i] + 1);
	printf("\n");

	playgame(myinput, 9, outbuf, 8, 9, 100);
	for (int i = 0; i < 8; i++)
		printf("%d", outbuf[i] + 1);
	printf("\n");

	playgame(myinput, 9, outbuf, 2, 1000000, 10000000);
	long long a = outbuf[0] + 1, b = outbuf[1] + 1;
	printf("%lld\n", a * b);
}
