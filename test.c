#include <stdio.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <sys/time.h>
#include <stdint.h>
#include <string.h>




const char * post_market = "\"postMarketPrice\":{\"raw\":";
const char * reg_market = "\"regularMarketPrice\":{\"raw\":";
const char * pre_market ="";
const char * reg_chg_market ="";
const char * chg_pcnt_reg_market="";
const char * end= ",\"fmt\":";


char * post_price = NULL;
char * reg_price = NULL;


char* fwrite_to_string(const void* data, size_t size) {
   
    char* buffer = (char*)malloc(size + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        return NULL;
    }

   
    FILE* stream = open_memstream(&buffer, &size);
    if (stream == NULL) {
        fprintf(stderr, "Failed to open memory stream.\n");
        free(buffer);
        return NULL;
    }

   
    size_t written = fwrite(data, size, 1, stream);
    fclose(stream);

    
    buffer[size] = '\0';

    return buffer;
}


char* remove_backslash(const char* str) {
    size_t len = strlen(str);
    char* result = (char*)malloc(len + 1); 
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        return NULL;
    }

    const char* read_ptr = str;
    char* write_ptr = result;
    while (*read_ptr) {
        if (*read_ptr == '\\' && *(read_ptr + 1) == '\"') {
            read_ptr++; 
        }
        *write_ptr++ = *read_ptr++;
    }
    *write_ptr = '\0'; 

   

    return result;
}

char* remove_newlines(const char* str) {
    if (str == NULL) {
        return NULL;
    }

    size_t len = strlen(str);
    char* result = (char*)malloc(len + 1);  
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        return NULL;
    }

    size_t j = 0; 

   
    for (size_t i = 0; i < len; i++) {
        
        if (str[i] == '\n' || str[i] == '\r' || str[i] == '\0') {
            continue;
        }

        
        result[j++] = str[i];
    }

    
    result[j] = '\0';
    
    return result;
}

char* concat(const char *s1, const char *s2)
{
    char *result = malloc(strlen(s1) + strlen(s2) + 1); 
   
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

char* find_middle_string(const char* str, const char* start_substr, const char* end_substr) {
    
  char* start_pos = strstr(str, start_substr);
    if (start_pos == NULL) {
        // Start substring not found
        return NULL;
    }

   
    start_pos += strlen(start_substr);

    
    char* end_pos = strstr(start_pos, end_substr);
    if (end_pos == NULL) {
        
        return NULL;
    }

  
    size_t middle_len = end_pos - start_pos;

    
    char* middle_str = (char*)malloc(middle_len + 1);
    if (middle_str == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        return NULL;
    }

    
    strncpy(middle_str, start_pos, middle_len);
    middle_str[middle_len] = '\0'; 

    return middle_str;
}


void process_html(const char *html_string){

  if(post_price!=NULL && reg_price!=NULL){

    

  }else{
    char * processed_string = remove_newlines(remove_backslash(html_string));
    
   
    post_price =  find_middle_string(processed_string, post_market, end);
    reg_price  =  find_middle_string(processed_string, reg_market, end);
    

    

    
    

    
    

     
    free(processed_string);
  }



}


size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t total_size = size * nmemb;
    
    size_t max_print_size = 10024; 

    
    if (total_size > max_print_size) {
       
        total_size = max_print_size;
    }

   
    // fwrite(contents, 1, total_size, stdout);
    char *html_string = fwrite_to_string(contents, total_size);
    process_html(html_string);


    return (size * nmemb);
}

int main(int argc, char *argv[]) {

  
  if(argc < 2){

    printf("Usage: <linux_finance aapl>\n");
    
    return 0;
  }

  

    char * url = concat( "https://finance.yahoo.com/quote/" , argv[1]); 
	
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();

   

    if (curl) {

	curl_easy_setopt(curl, CURLOPT_ACCEPT_ENCODING, "");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_USERAGENT, "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/81.0");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);

        res = curl_easy_perform(curl);
        if (res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

     gettimeofday(&stop, NULL);

     

     if(reg_price!=NULL){
       printf("Reg_Price: %s\n", reg_price);
      }

     if(post_price!=NULL){
        printf("Post_Price: %s\n", post_price);
      }
     printf("\n");
     printf("Process took %lu u-seconds.\n", (stop.tv_sec - start.tv_sec) * 1000000 + stop.tv_usec - start.tv_usec); 

    free(reg_price);
    free(post_price);
    free(url);
    curl_global_cleanup();
    return 0;
}



