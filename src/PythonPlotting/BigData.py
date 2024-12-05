import pandas as pd
import matplotlib.pyplot as plt

filepath = 'Tables/Program.BigDataArrayAndListSortComparisonBenchmark.csv'

df_raw = pd.read_csv(filepath, sep = ';', thousands = ',')

df = df_raw[['Method', 'Element_Count', 'Mean [ms]', 'Ratio']]

# Running time
pvt = df[df['Method'].str.find('sort') == -1].pivot_table(values = 'Mean [ms]', index = 'Element_Count', columns = 'Method')
pvt.plot()
plt.xlabel('Element count')
plt.ylabel('Running time, ms')
plt.title('Array and list sorting algorithms performance')
plt.savefig('Array and list fast sorts comparison.png')

# Ratio
pvt = df[df['Method'] != 'Array.sort'].pivot_table(values = 'Ratio', index = 'Element_Count', columns = 'Method')
pvt.plot()
plt.xlabel('Element count')
plt.ylabel('Running time to Array.sort time ratio')
plt.title('Impemented algorithms comparison to built-in array sort performance')
plt.savefig('Implemented algorithms comparison to built-in array sort.png')

# Running time
pvt = df.pivot_table(values = 'Mean [ms]', index = 'Element_Count', columns = 'Method')
pvt.plot()
plt.xlabel('Element count')
plt.ylabel('Running time, ms')
plt.title('Built-in sorts are too fast')
plt.savefig('Built-in sorts are too fast.png')