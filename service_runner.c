#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>



#define BUFFER_SIZE 1024




char *run_command(const char *command, const char *arguments) {
    FILE *fp;
    char *response = NULL;
    char line[1024]; 

    
    char full_command[1024]; 
    snprintf(full_command, sizeof(full_command), "%s %s", command, arguments);

   
    fp = popen(full_command, "r");
    if (fp == NULL) {
        printf("Failed to run command\n");
        return NULL;
    }

    
    size_t total_size = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        size_t line_length = strlen(line);
        char *new_response = realloc(response, total_size + line_length + 2); 
        if (new_response == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            free(response);
            pclose(fp);
            return NULL;
        }
        response = new_response;
        strcpy(response + total_size, line);
        total_size += line_length;
	// response[total_size++] = '\n'; 
        response[total_size] = '\0';  
    }

   
    pclose(fp);

    return response;
}

char* concat_strings(const char* str1, const char* str2) {
    
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t total_len = len1 + len2 + 1; 

    
    char* result = (char*)malloc(total_len * sizeof(char));
    if (result == NULL) {
        fprintf(stderr, "Memory allocation failed.\n");
        return NULL;
    }

    
    strcpy(result, str1);

   
    strcat(result, str2);

    return result;
}



int main(int argc, char *argv[] ) {

  if(argc < 2){
    printf("Usage: commmand app port");

  }

    size_t PORT = atoi(argv[2]); 
    char * command = argv[1];
    
    char buffer[BUFFER_SIZE];
    char *final_response = NULL; 
    char resp[] = "HTTP/1.0 200 OK\r\n"
                  "Server: webserver-c\r\n"
      "Content-type: text/html\r\n\r\n";
      //"<html>hello, world</html>\r\n";

    
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) {
        perror("webserver (socket)");
        return 1;
    }
    printf("socket created successfully\n");

    
    struct sockaddr_in host_addr;
    int host_addrlen = sizeof(host_addr);

    host_addr.sin_family = AF_INET;
    host_addr.sin_port = htons(PORT);
    host_addr.sin_addr.s_addr = htonl(INADDR_ANY);

   
    struct sockaddr_in client_addr;
    int client_addrlen = sizeof(client_addr);

    
    if (bind(sockfd, (struct sockaddr *)&host_addr, host_addrlen) != 0) {
        perror("webserver (bind)");
        return 1;
    }
    printf("socket successfully bound to address\n");

    
    if (listen(sockfd, SOMAXCONN) != 0) {
        perror("webserver (listen)");
        return 1;
    }
    printf("server listening for connections\n");

    for (;;) {
        
        int newsockfd = accept(sockfd, (struct sockaddr *)&host_addr,
                               (socklen_t *)&host_addrlen);
        if (newsockfd < 0) {
            perror("webserver (accept)");
            continue;
        }
        printf("connection accepted\n");

        
        int sockn = getsockname(newsockfd, (struct sockaddr *)&client_addr,
                                (socklen_t *)&client_addrlen);
        if (sockn < 0) {
            perror("webserver (getsockname)");
            continue;
        }

        
        int valread = read(newsockfd, buffer, BUFFER_SIZE);
        if (valread < 0) {
            perror("webserver (read)");
            continue;
        }

        
        char method[BUFFER_SIZE], uri[BUFFER_SIZE], version[BUFFER_SIZE];
        sscanf(buffer, "%s %s %s", method, uri, version);
        printf("[%s:%u] %s %s %s\n", inet_ntoa(client_addr.sin_addr),
	       ntohs(client_addr.sin_port), method, version, uri);

	int valwrite = -1;


	final_response = concat_strings(resp, "<html>");
	char *temp = final_response; 
	final_response = concat_strings(final_response, run_command(command, uri+1));
	free(temp);
	temp= final_response;
       	final_response = concat_strings(final_response, "</html>");
	free(temp);


	  
	printf("%s\n", final_response);
	
         valwrite = write(newsockfd, final_response, strlen(final_response));
	
	if (valwrite < 0) {
            perror("webserver (write)");
            continue;
        }
	

        close(newsockfd);
    }

    return 0;
}
