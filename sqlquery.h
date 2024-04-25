
/*
 * src/test/examples/testlibpq.c
 *
 *
 * testlibpq.c
 *
 *      Test the C version of libpq, the PostgreSQL frontend library.
 */
#include <stdio.h>
#include <stdlib.h>
#include "libpq-fe.h"
#include "helpful_string.h"

char * connection_str="user=mrfinance dbname=analysis password = %v.8DwUp$Ha(X/YQ";
//char * connection_str="user=postgres dbname=analysis password =qwerty";

static void
do_exit(PGconn *conn)
{
    PQfinish(conn);
    exit(1);
}

char * get_value(PGresult * res){

    int rows = PQntuples(res);
    int columns = PQnfields(res);
    char * result = malloc( columns * rows * 2000);
    int offset =0; 

    printf("Rows %d\n", rows);
    printf("Column %d\n", columns);

    if(rows>0){
    for(int i=0; i<rows; i++) {
      
     
      
      for(int k=0; k<columns;k++){ 

	

	char * pq_value = PQgetvalue(res, i,k);
	
        

	  offset +=  sprintf(result+offset,"%s", pq_value);
	}        
      

      
     
        }
    }
    else{

         sprintf(result+offset,"%.10s","-");
    }
   

    return result;


}




char * get_singular_data(char ticker[], char * identifier, char * statement  ){

      PGconn *conn = PQconnectdb(connection_str);

      if (PQstatus(conn) == CONNECTION_BAD) {
        
        fprintf(stderr, "Connection to database failed: %s\n",
            PQerrorMessage(conn));
        do_exit(conn);
       }

      char * result = malloc(1000);
      int result_offset=0;


      for(int year = 2019; year <=2023; year++ ){

          char * query = malloc(1000);

	  int offset =0;
	  offset+= sprintf(query + offset,"select CASE "
			   "  WHEN ABS(value) > 500000 AND ABS(value) < 500000000 THEN "
                           " (value/1000000.0)::numeric(10,2) || 'M' "
                           " WHEN ABS(value) > 500000000 THEN "
                    	   " (value/1000000000.0)::numeric(10,2) || 'B' "
                           " ELSE "
                           " value::text END "

			   " from statements inner join companies"
			   " on companies.company_id= statements.company_id"
		           "           where " 
                           "    provider_id=2"
                           "    and statement_type='%s'"
                           "    and identifier = '%s'"
                           "    and companies.ticker =UPPER('%s')"
                           "    and date_part('year', date) = %d", statement, identifier, ticker, year);

	  
	 
	   PGresult *res = PQexec(conn, query);    
    
	   if (PQresultStatus(res) != PGRES_TUPLES_OK) {

	     fprintf(stderr, "SELECT failed: %s", PQerrorMessage(conn));       
	     PQclear(res);
	     do_exit(conn);
	   }


	   
	        
    
	       char * value = get_value(res);
	       
	      
	       result_offset += sprintf(result + result_offset,"%.10s  ", value);
	   

	       
	  PQclear(res);
	  free(query);
	  free(value);
      }

      
      PQfinish(conn);

     


      return result; 


}


char * get_statement_data(char ticker[])
{

    char * result = malloc(1000000);
    int offset =0;

    char * revenue = get_singular_data(ticker, "Sales/Revenue Sales/Revenue", "YEARLY-INCOME-STATEMENT");
    char * interest = get_singular_data(ticker, "Interest Income Interest Income", "YEARLY-INCOME-STATEMENT");
    char * asset = get_singular_data(ticker, "Total Assets Total Assets", "YEARLY-BALANCE-SHEET");
    char * net_income = get_singular_data(ticker, "Net Income Net Income", "YEARLY-INCOME-STATEMENT");
    char * debt = get_singular_data(ticker, "Total Liabilities Total Liabilities", "YEARLY-BALANCE-SHEET");
    char * cashflow = get_singular_data(ticker, "Net Operating Cash Flow Net Operating Cash Flow", "YEARLY-CASHFLOW-STATEMENT");
    char * free_cashflow = get_singular_data(ticker, "Free Cash Flow Free Cash Flow", "YEARLY-CASHFLOW-STATEMENT");
    
    

     offset += sprintf(result+offset,"<fundamental>");

     offset += sprintf(result+offset, "<year>Year: 2019 2020 2021 2022 2023</year>");
     
     if(strlen(interest) <=15){
       offset += sprintf(result+offset,"<revenue>Revenue: %s</revenue>",revenue);
       
    }else if(strlen(revenue)<=15){
       offset += sprintf(result+offset,"<revenue>Interest: %s</revenue>",interest);
    }else{
      offset += sprintf(result+offset,"<interest>Iterest: %s</interest>",interest);
      offset += sprintf(result+offset,"<revenue>Revenue: %s</revenue>",revenue);

    }

    
    
    offset += sprintf(result+offset,"<free_cashflow>Free Cashflow: %s</free_cashflow>",free_cashflow);
    offset += sprintf(result+offset,"<debt>Debt: %s</debt>",debt);
    offset += sprintf(result+offset,"<cashflow>Cashflow: %s</cashflow>",cashflow);
    offset += sprintf(result+offset,"<asset>Asset %s</asset>",asset);
    offset += sprintf(result+offset,"<net_income>Net Income %s</net_income>",net_income);
    offset += sprintf(result+offset,"</fundamental>");


    free(interest);
    free(revenue);
    free(debt);
    free(cashflow);
    free(free_cashflow);
    free(asset);
    free(net_income);

    printf("%s\n",result);

    return result;
}

char * get_type_data(char ticker[])
{

    char * result = malloc(2000);
  
    PGconn *conn = PQconnectdb(connection_str);

    if (PQstatus(conn) == CONNECTION_BAD) {
        
        fprintf(stderr, "Connection to database failed: %s\n",
            PQerrorMessage(conn));
        do_exit(conn);
    }

    char * query = malloc(1000);

    int offset =0;
    offset+= sprintf(query + offset,"SELECT substring(description for 500) FROM companies where ticker =UPPER('%s') and description <> ''", ticker); 
    
    PGresult *res = PQexec(conn, query);    
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {

        fprintf(stderr, "SELECT failed: %s", PQerrorMessage(conn));     
        PQclear(res);
        do_exit(conn);
    }       
    

    offset =0; 
    offset += sprintf(result+offset,"<description>");
    offset += sprintf(result+offset,"<type>");

    char * xml = get_value(res);

    xml = replace(xml, "&", ""); 

    offset += sprintf(result+offset, xml);

    

    offset += sprintf(result+offset,"</type>");
    offset += sprintf(result+offset,"</description>");

    printf("%s\n", result);
    
    free(xml);
    free(query);
    PQclear(res);
    PQfinish(conn);

   

    

    return result;
}
