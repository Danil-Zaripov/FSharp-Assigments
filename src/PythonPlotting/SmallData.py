import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

filepath = 'Tables/Program.SmallDataListAndArraySortComparisonBenchmark.csv'

df_raw = pd.read_csv(filepath, sep = ';', thousands=',')

df = df_raw[['Method', 'Element_Count', 'Mean [s]', 'Ratio']]

#Running time
pvt = df.pivot_table(values = 'Mean [s]', index = 'Element_Count', columns = 'Method')
pvt.plot()
plt.xlabel('Element count')
plt.ylabel('Running time, s')
plt.title('Array and list sorting algorithms performance')
plt.savefig('Slow sorts running time.png')

pvt['Insertion Sort'] = pvt['List_Insertion_Sort'].div(pvt['Array_Insertion_Sort'])
pvt['Bubble Sort'] = pvt['List_Bubble_Sort'].div(pvt['Array_Bubble_Sort'])
pvt.plot(y = ['Bubble Sort', 'Insertion Sort'])
plt.xlabel('Element count')
plt.ylabel('List sort time / Array sort time')
plt.title('List sort to array sort performance ratio')
plt.savefig('List to array slow sort ratio.png')

# Ratio
pvt = df.pivot_table(values = 'Ratio', index = 'Element_Count', columns = 'Method')
pvt.plot()
plt.xlabel('Element count')
plt.ylabel('Running time time ratio')
plt.title('Comparison to Array bubble sort performance')
plt.savefig('Array bubble sort comparison.png')