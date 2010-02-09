#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include <ctype.h>

struct TBL_DS
{
  char *key_string;
  char *data_string;
};
typedef struct TBL_DS table;

class TBL
{
  public : int TBL_SIZE[3];
           table* lookup_tbl[3];
           int idx;

	       TBL();
           void Add_into_table(char *key, char *data, int table);
           void Read_table();
           int Initialize_table();
           char* Search_table(char* key, int table);
};

TBL::TBL()
{
  Initialize_table();
}
void TBL::Add_into_table(char *key, char *data, int table)
{
  lookup_tbl[table][idx].key_string = (char *)malloc(sizeof(key) + sizeof(char) + 1);
  strcpy(lookup_tbl[table][idx].key_string, key);
  lookup_tbl[table][idx].data_string = (char *)malloc(sizeof(data) + sizeof(char) + 1);  
  strcpy(lookup_tbl[table][idx++].data_string, data);
}

void TBL::Read_table()
{
  int i,j;
  for(j = 0; j < 3; j++)
  {
    printf("TABLE %d\r\n", j+1); 
    for(i = 0; i < TBL_SIZE[j]; i++)
    {
      printf("%d : lookup item \"%s\" new item \"%s\"\r\n", i+1, lookup_tbl[j][i].key_string, lookup_tbl[j][i].data_string); 
    }
  }
}

int TBL::Initialize_table()
{
  FILE* input_file;
  char line[80];
  int count = 0, i;
  
  for(i = 0; i < 3; i++)
  {
    switch(i) 
    {
      case 0 : input_file = fopen("./cell/integer_simd_table.txt", "r");
               if(input_file == NULL)
               {
                 printf("Error: The file \"integer_simd_table.txt\" does not exists\r\n");
                 return 1;
               }  
               break;

      case 1 : input_file = fopen("./cell/float_simd_table.txt", "r");
               if(input_file == NULL)
               {
                 printf("Error: The file \"float_simd_table.txt\" does not exists\r\n");
                 return 1;
               }  
               break;
   
      case 2 : input_file = fopen("./cell/double_simd_table.txt", "r");
               if(input_file == NULL)
               {
                 printf("Error: The file \"double_simd_table.txt\" does not exists\r\n");
                 return 1;
               }  
               break;
 
      default : printf("Error: Wrong table\r\n");
                input_file = NULL;
                return 1;
                break;
    }

    
    
    count = 0; 
    while (fgets(line, 80, input_file) != NULL) count++;
    printf("The number of lines in the file %d\r\n", count);
  
    TBL_SIZE[i] = count;

    lookup_tbl[i] = (table *)malloc(TBL_SIZE[i] * sizeof(table));
    idx = 0;
    printf("Initialize the table \r\n");
 
    rewind(input_file);

    while(fgets(line, 80, input_file) != NULL)
    {
      char key[30];
      char data[30];
      sscanf(line, "%s %s", key, data);
      Add_into_table(key, data, i);
    }

    fclose(input_file);
  }
  return 0;
}

char* TBL::Search_table(char* key, int table)
{
  int i = 0;
  printf("key = %s\r\n", key); 
		
  for(i = 0; i < sizeof(key) - 1; i++) 
	key[i] = tolower(key[i]);
	  
 
  for(i = 0; i < TBL_SIZE[table]; i++)
  {
    if(strcmp(key, lookup_tbl[table][i].key_string) == 0)
     {
       printf("Found\r\n");
       return lookup_tbl[table][i].data_string;
     }
  }
 
 return "";
}

