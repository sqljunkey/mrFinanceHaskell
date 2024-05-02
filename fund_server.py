import socket
import yfinance as yf


# Define socket host and port
SERVER_HOST = '0.0.0.0'
SERVER_PORT = 9091

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
     # Get the years from the income statement DataFrame

        stock = yf.Ticker(symbol)
        
        years = stock.income_stmt.columns
        info = stock.info.get("longBusinessSummary","N/A")
        
        

        # Initialize variables with default values
        net_income_row = {}
        revenue = {}
        total_debt = {}
        total_asset = {}
        free_cash_flow = {}
        stock_repurchase = {}

        # Get values if located
        if 'Net Income' in stock.income_stmt.index:
            net_income_row = stock.income_stmt.loc['Net Income']
        if 'Total Revenue' in stock.income_stmt.index:
            revenue = stock.income_stmt.loc['Total Revenue']
        if 'Total Debt' in stock.balance_sheet.index:
            total_debt = stock.balance_sheet.loc['Total Debt']
        if 'Total Assets' in stock.balance_sheet.index:
            total_asset = stock.balance_sheet.loc['Total Assets']
        if 'Free Cash Flow' in stock.cashflow.index:
            free_cash_flow = stock.cashflow.loc['Free Cash Flow']
        if 'Repurchase Of Capital Stock' in stock.cashflow.index:
            stock_repurchase = stock.cashflow.loc['Repurchase Of Capital Stock']

        # Build XML content
        xml_content = '<income_statement>\n'
        xml_content += '\t\t<years>              ' + '    '.join(str(year.year) for year in years) + '</years>\n'
        xml_content += '\t\t<revenue>Revenue      : ' + '  '.join(str(format_number(revenue.get(year, 0.0))) for year in years) + '</revenue>\n'
        xml_content += '\t\t<net_income>Net Income   : ' + '  '.join(str(format_number(net_income_row.get(year, 0.0))) for year in years) + '</net_income>\n'
        xml_content += '\t\t<free_cashflow>FCF          : ' + '  '.join(str(format_number(free_cash_flow.get(year, 0.0))) for year in years) + '</free_cashflow>\n'
        xml_content += '\t\t<stock_repurchase>Stock Buyback: ' + '  '.join(str(format_number(stock_repurchase.get(year, 0.0))) for year in years) + '</stock_repurchase>\n'
        xml_content += '\t\t<total_debt>Total Debt   : ' + '  '.join(str(format_number(total_debt.get(year, 0.0))) for year in years) + '</total_debt>\n'
        xml_content += '\t\t<total_asset>Total Asset  : ' + '  '.join(str(format_number(total_asset.get(year, 0.0))) for year in years) + '</total_asset>\n'
        xml_content += f'\t\t<type>{info[0:1000]}....</type>\n'
        xml_content += '</income_statement>'
        
      
        xml_content += '</income_statement>'
        
        return xml_content
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

