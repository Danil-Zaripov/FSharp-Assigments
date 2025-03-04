import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

filepath = 'Tables/Program.SmallDataListAndArraySortComparisonBenchmark.csv'

df_raw = pd.read_csv(filepath, sep = ';', thousands=',')

df = df_raw[['Method', 'Element_Count', 'Mean [s]', 'Ratio']].rename(columns = {'Method' : 'Функция'})

russify = {
    'Method' : 'Функция',
    'Array_Merge_Sort' : 'Сорторовка слиянием на массиве',
    'Array_Quick_Sort' : 'Быстрая сортировка на массиве',
    'Array_Bubble_Sort' : 'Сортировка пузырьком на массиве',
    'Array_Insertion_Sort' : 'Сортировка вставками на массиве',
    'List_Merge_Sort' : 'Сортировка слиянием на списке',
    'List_Quick_Sort' : 'Быстрая сортировка на списке',
    'List_Bubble_Sort' : 'Сортировка пузырьком на списке',
    'List_Insertion_Sort' : 'Сортировка вставками на списке',
    'Insertion Sort' : 'Сортировка вставками',
    'Quick Sort' : 'Быстрая сортировка',
    'Merge Sort' : 'Сортировка слиянием',
    'Bubble Sort' : 'Сортировка пузырьком',
    'Built-in sort' : 'Встроенная сортировка',
    'Array Merge Sort' : 'Сорторовка слиянием на массиве',
    'Array Quick Sort' : 'Быстрая сортировка на массиве',
    'Array Bubble Sort' : 'Сортировка пузырьком на массиве',
    'Array Insertion Sort' : 'Сортировка вставками на массиве',
    'List Merge Sort' : 'Сортировка слиянием на списке',
    'List Quick Sort' : 'Быстрая сортировка на списке',
    'List Bubble Sort' : 'Сортировка пузырьком на списке',
    'List Insertion Sort' : 'Сортировка вставками на списке'
}

#Running time
pvt = df.pivot_table(values = 'Mean [s]', index = 'Element_Count', columns = 'Функция')
pvt.rename(columns = russify, inplace=True)
pvt.plot()
plt.xlabel('Кол-во элементов')
plt.ylabel('Время работы, с')
plt.title('Производительность сортировок на списках и массивах')
plt.savefig('Slow sorts running time.png')

pvt[russify['Insertion Sort']] = pvt[russify['List_Insertion_Sort']].div(pvt[russify['Array_Insertion_Sort']])
pvt[russify['Bubble Sort']] = pvt[russify['List_Bubble_Sort']].div(pvt[russify['Array_Bubble_Sort']])
pvt.plot(y = [russify['Bubble Sort'], russify['Insertion Sort']])
plt.xlabel('Кол-во элементов')
plt.ylabel('Отношение, кол-во раз')
plt.title('Сравнение производительности сортировок на массиве\nс сортировками на списках')
plt.savefig('List to array slow sort ratio.png')

# Ratio
pvt = df.pivot_table(values = 'Ratio', index = 'Element_Count', columns = 'Функция')
pvt.rename(columns = russify, inplace=True)
pvt.plot()
plt.xlabel('Кол-во элементов')
plt.ylabel('Отношение, кол-во раз')
plt.title('Сравнение с пузырьковой сортировкой на массиве')
plt.savefig('Array bubble sort comparison.png')