Лабораторная №2. Деревья
=======

Какие следующие действия:
- реализовать вложенность структур v
- написать тесты вложенных структур v
- другие численные типы? x
- написать Функцию Построить список, содержащий все ключи в объектах дерева v
- сериализация обратно в строку (начать с сохранения в файл)
- генерация случайного

Как добавить простое строковое ключ/значение  
Самый простой вариант, как и описано в объявлении типа - список кортежей (строка, JSON). Если добавить для "value1" ещё объявление строки, получится:  
  
`data JSON = Object [(String, JSON)] | String String`  

`json = Object [("key1", String "value1")]`  
  
Но, если хочется, можно взять вместо списка Data.Map. Правда, на лекциях про это ничего не было.  

### Реализовать:
1. Тип данных для представления JSON-объектов. Будем ещё его называть деревом, так как по сути
это оно и есть, и задания больше относятся к работе с деревом, нежели с JSON
2. Разбор JSON-строки в объект
3. Функцию обработки древовидной структуры в соответствии с вариантом задания
4. Функцию генерации случайного JSON-объекта
5. Сериализацию объекта обратно в строку

### Генерация случайного объекта
Для получения последовательности случайных чисел можно использовать генератор псевдослучайных чисел 
типа `StdGen` и функцию `random`, или функции получения случайных значений из внешнего мира `randomIO`, `randomRIO`
и т.д.
В первом случае сложность может возникнуть в правильной передаче нового состояния вашего генератора
дальнейшим вычислениям. В этом случае удобно обернуть генерирующие функции в монаду `State`, маленький пример есть в шаблоне.
Во втором случае придётся работать внутри монады IO, что может быть неприятно.
Для многих заданий полезно причислить тип JSON к классу типов `Monoid`

### Тесты
Нужно будет написать тесты, в которых проверить работу программы на нескольких
модельных примерах, показывающих правильность реализованного алгоритма, а также на
случайно сгенерированном дереве.

### Определения
JSON-объекты представлены следующими типами (json.org):
- объект, содержащий пары ключ - значение (другой JSON-объект)
- массив из JSON-объектов
- примитивные типы: null, number, string, true, false

Глубиной вершины дерева называется длина пути в эту вершину из корня. Глубиной (высотой)
дерева называется максимальная глубина его вершин. Листом или терминальной вершиной
дерева называется вершина, не имеющая поддеревьев. Степенью вершины называется число
исходящих из неё ветвей. Степенью дерева называется максимальная степень его вершин.
Шириной уровня дерева называется число вершин на данной глубине. Шириной дерева
называется максимальная ширина по всем уровням. Подобие деревьев отличается от равенства
возможным несовпадением значений данных, хранящихся в узлах.

### Варианты заданий
1. Найти количество элементов во всех массивах
2. Построить список, содержащий все ключи в объектах дерева
3. Определить ширину дерева
4. Определить максимальную глубину у числовых элементов
5. Определить глубину дерева
6. Определить степень дерева
7. Определить число нетерминальных вершин дерева (объекты и массивы)
8. Определить число вершин дерева, степень которых совпадает со степенью дерева
9. Определить уровень дерева, на котором находится максимальное число вершин
10. Определить значение листа дерева (примитвный тип), имеющего минимальную глубину
11. Определить значение нетерминальной вершины дерева с максимальной глубиной
12. Проверить, все ли числа в JSON находятся в заданном диапазоне
13. Узнать, подобны ли два JSON-объекта
14. Проверить монотонность возрастания ширины уровня дерева
15. Проверить монотонность возрастания длины массивов в зависимости от уровня их вложенности
16. Найти сумму всех чисел в JSON
17. Проверить, находятся ли все числа на одном уровне
18. Увеличить значения всех чисел в JSON на их уровень
19. Увеличить на единицу значения листьев дерева, находящихся на нечётных уровнях
20. Добавить произвольный примитивный элемент в дерево
21. Найти количество вхождений элемента примитивного типа в дерево
22. Удалить элемент примитивного типа из дерева
23. Проверить, является ли дерево симметричным (равным своему отражению, объекты в JSON не отражаются)
24. Проверить, является ли дерево самоподобным (подобным своему отражению, объекты в JSON не отражаются)
25. Написать функцию, которая объединяет два JSON-объекта или JSON-массива
26. Написать функцию, которая принимает JSON-объект и возвращает новый, где ключи — значения, а значения — ключи
27. Написать функцию, которая принимает список ключей и проверяет, все ли из них есть в JSON
28. Составить хеш: значение примитивного типа в JSON, сколько раз оно встречается


