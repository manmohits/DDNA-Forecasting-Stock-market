import datetime
import random 
import math 
import time
from operator import add

VOLATILITY = 0.3672
RISK_FREE_RATE = 0.0024
STRIKE_PRICE = 7
CURRENT_VALUE = 7.37

T = (datetime.date(2016,11,18) - datetime.date(2016,10,17)).days / 365.0

discount_factor = math.exp(-RISK_FREE_RATE * T)


def call_payoff(asset_price,STRIKE_PRICE):
    return max(0.0,asset_price - STRIKE_PRICE)

def sim_option_price(seed):
    random.seed(seed)
    asset_price = CURRENT_VALUE * math.exp((RISK_FREE_RATE - 0.5 * VOLATILITY**2) * T + VOLATILITY * math.sqrt(T) * random.gauss(0,1.0))
    return call_payoff(asset_price,STRIKE_PRICE)


seeds = sc.parallelize([time.time() + i for i in xrange(10000)])
results = seeds.map(sim_option_price)
sum = results.reduce(add)
price = discount_factor * (sum / float(10000))

'${:,.2f}'.format(price)