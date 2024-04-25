import socket
import yfinance as yf
import yahoo_fin.stock_info as si

# Define socket host and port fhkDys1owrtTnmuJ7Yr3
SERVER_HOST = '0.0.0.0'
SERVER_PORT = 9092

# Create socket
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
server_socket.bind((SERVER_HOST, SERVER_PORT))
server_socket.listen(1)
print('Listening on port %s ...' % SERVER_PORT)

def format_number(n):
    if n == "N/A":
        return "N/A"
    suffixes = ['', 'K', 'M', 'B', 'T']
    magnitude = 0
    while abs(n) >= 1000:
        magnitude += 1
        n /= 1000.0
    return '{:.1f}{}'.format(n, suffixes[magnitude])

def decimal_to_percentage(decimal):
    if decimal == "N/A":
        return "N/A"
    percentage = decimal * 100
    return '{:.2f}%'.format(percentage)

def format_number_with_sign(num):
    if num >= 0:
        return '+{:.2f}'.format(num)
    else:
        return '{:.2f}'.format(num)

def format_two_decimal(num):
    return '{:.2f}'.format(num)



def get_stock_details(symbol):
    try:
        stock = yf.Ticker(symbol)
        price = si.get_live_price(symbol)

          
        data = stock.info
        change = ((price/data.get("regularMarketPreviousClose", "N/A"))-1.0)*100.0;

        
        xml_response = f'<stock>\n'
        
        xml_response += f'\t<long_name>{data.get("longName",data.get("shortName","N/A"))}</long_name>\n'
        xml_response += f'\t<current_price>{format_two_decimal(price)}</current_price>\n'
        xml_response += f'\t<percentage>{format_number_with_sign(change)}</percentage>\n'
        xml_response += f'\t<beta>{data.get("beta", "N/A")}</beta>\n'
        xml_response += f'\t<dividend_yield>{decimal_to_percentage(data.get("dividendYield", "N/A"))}</dividend_yield>\n'
        xml_response += f'\t<market_cap>{format_number(data.get("marketCap", "N/A"))}</market_cap>\n'
        xml_response += f'\t<p_e_ratio>{data.get("forwardPE", "N/A")}</p_e_ratio>\n'
        xml_response += f'\t<week_52_range>{data.get("fiftyTwoWeekLow")} - {data.get("fiftyTwoWeekHigh")}</week_52_range>\n'
        xml_response += f'\t<volume>{data.get("volume", "N/A")}</volume>\n'
        xml_response += f'\t<average_volume>{data.get("averageVolume", "N/A")}</average_volume>\n'
        xml_response += '</stock>'
        return xml_response
    except Exception as e:
        return f"<error>{e}</error>"

while True:
    # Wait for client connections
    client_connection, client_address = server_socket.accept()

    # Get the client request
    request = client_connection.recv(1024).decode()
    print(request)

    # Extract stock symbol from the request
    stock_symbol = request.split()[1][1:]  # Assuming the request is like "GET /stock_symbol HTTP/1.1"
    
    # Get stock details
    stock_details_xml = get_stock_details(stock_symbol)

    # Send HTTP response
    response = f'HTTP/1.0 200 OK\nContent-Type: application/xml\n\n{stock_details_xml}'
    client_connection.sendall(response.encode())
    client_connection.close()

# Close socket
server_socket.close()

