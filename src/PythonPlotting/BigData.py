import pandas as pd
import matplotlib.pyplot as plt

filepath = 'Tables/Program.BigDataArrayAndListSortComparisonBenchmark.csv'

df_raw = pd.read_csv(filepath, sep = ';', thousands = ',')

df = df_raw[['Method', 'Element_Count', 'Mean [ms]', 'Ratio']].rename(columns = {'Method' : 'Функция'})

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

# Running time
pvt = df[df['Функция'].str.find('sort') == -1].pivot_table(values = 'Mean [ms]', index = 'Element_Count', columns = 'Функция')
pvt.rename(columns = russify, inplace=True)
pvt.plot()
plt.xlabel('Кол-во элементов')
plt.ylabel('Время работы, мс')
plt.title('Производительность сортировок на списках и массивах')
plt.savefig('Array and list fast sorts comparison.png')

# List to Array Ratio
pvt = df.pivot_table(values = 'Mean [ms]', index = 'Element_Count', columns = 'Функция')
pvt['Quick Sort'] = pvt['List_Quick_Sort'].div(pvt['Array_Quick_Sort'])
pvt['Merge Sort'] = pvt['List_Merge_Sort'].div(pvt['Array_Merge_Sort'])
pvt['Built-in sort'] = pvt['List.sort'].div(pvt['Array.sort'])
pvt.rename(columns = russify, inplace=True)
pvt.plot(y = [russify['Quick Sort'], russify['Merge Sort'], russify['Built-in sort']])
plt.xlabel('Кол-во элементов')
plt.ylabel('Отношение, кол-во раз')
plt.title('Сравнение производительности сортировок на массиве\nс сортировками на списках')
plt.savefig('List to array fast sorts ratio.png')

# Ratio
pvt = df.pivot_table(values = 'Mean [ms]', index = 'Element_Count', columns = 'Функция')
pvt['List Merge Sort'] = pvt['List_Merge_Sort'].div(pvt['Array_Quick_Sort'])
pvt['List Quick Sort'] = pvt['List_Quick_Sort'].div(pvt['Array_Quick_Sort'])
pvt['Array Merge Sort'] = pvt['Array_Merge_Sort'].div(pvt['Array_Quick_Sort'])
pvt['Array Quick Sort'] = pvt['Array_Quick_Sort'].div(pvt['Array_Quick_Sort'])
pvt.drop(columns = ['List_Merge_Sort', 'Array_Quick_Sort', 'List_Quick_Sort', 'Array_Merge_Sort'], inplace = True)
pvt.rename(columns = russify, inplace = True)
pvt.plot(y = [russify['List Quick Sort'], russify['List Merge Sort'], russify['Array Quick Sort'], russify['Array Merge Sort']])

plt.xlabel('Кол-во элементов')
plt.ylabel('Отношение, кол-во раз')
plt.title('Сравнение с быстрой сортировкой на массиве')
plt.savefig('Array quick sort performance comparison.png')