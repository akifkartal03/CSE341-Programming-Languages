#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
int yyerror(char *error);

/*This Linked List is to keep variables for set operation*/
typedef struct node_s
{
    int data;
    char name[30];
    struct node_s *next;
}node_t;

//global head;
node_t *head= NULL;
node_t *temp = NULL;
void addNewVariable(char name[30], int data){
    node_t *iter;
    iter=head;
    if (head==NULL) {
        head=(node_t *)malloc(sizeof(node_t));
        head->data=data;
        strcpy(head->name, name);
        head->next=NULL;
    }
    else{
        while(iter->next!=NULL){
            iter=iter->next;
        }
        node_t *temp1=(node_t *)malloc(sizeof(node_t));
        temp1->data=data;
        strcpy(temp1->name, name);
        temp1->next=iter->next;
        iter->next =temp1;
    }
}

int getDataOfVariable(char name[30]){
    node_t *iter;
    iter=head;
    while(iter != NULL){
        if(strcmp(iter->name, name) == 0){
            return iter->data;
		}
        iter=iter->next;
    }
    return -1;// if not found return -1

}
/*append operation on linked list*/
int* appendElementToList(int *list, int element){
    node_t *iter;
    iter=temp;
    if (temp==NULL) {
        temp=(node_t *)malloc(sizeof(node_t));
        temp->data=element;
        temp->next=NULL;
        list = (int *)malloc(sizeof(int)*2);
        list[0] = element;
        list[1] = -999;
    }
    else{
        int size = 0;
        while(iter->next!=NULL){
            size++;
            iter=iter->next;
        }
        size++;
        node_t *temp1=(node_t *)malloc(sizeof(node_t));
        temp1->data=element;
        temp1->next=iter->next;
        iter->next =temp1;
        list = (int*)(malloc(sizeof(int)*(size+2)));
        int i = 0;
        iter=temp;
        while(iter != NULL){
            list[i] = iter->data;
            i++;
            iter=iter->next;
		}
        list[i] = -999;
    }
    return list;
}

/*concat operation on linked list*/
int* concatTwoList(int *list1, int *list2){
    return list2;
}

int recursive_pow(int base,int exponent){
	int result;
	if (exponent==0 && base==0)
        return -1;
	if (exponent==0)
        return 1;
	else{
		result=base*recursive_pow(base, exponent-1);
	}
	return result;
}
