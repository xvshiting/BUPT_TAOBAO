from sklearn.ensemble import AdaBoostClassifier
import pandas as pd
from sklearn.naive_bayes import MultinomialNB


#训练并计算准确率 path：数据文件地址    rate：adaboost学习率
def train_test(path, rate):
    data = pd.read_csv(path)
    data_train = data.iloc[:, :-1]   #取属性
    type = data.iloc[:, -1]   #取类标签
    adaboost = AdaBoostClassifier(n_estimators=500, learning_rate=rate, algorithm='SAMME.R', random_state=None)
    model = adaboost.fit(data_train.iloc[:80000], type.iloc[:80000])   #前80000行作为训练集，剩下的作为测试集，用训练集训练模型
    return model.score(data_train.iloc[80000:], type.iloc[80000:])   #计算在测试集上的准确率

#训练得到模型，输入测试数据，并将预测结果出输出到文件   path_train：训练数据文件地址   path_test：测试数据文件地址
#         result：结果输出文件地址       rate：adaboost学习率

def test_result(path_train, path_test, result_path, rate):
    data = pd.read_csv(path_train)
    data_train = data.iloc[:, :-1]   #取属性
    type = data.iloc[:, -1]   #取类标签
    data_test = pd.read_csv(path_test)
    adaboost = AdaBoostClassifier(n_estimators=500, learning_rate=rate, algorithm='SAMME.R', random_state=None)
    model = adaboost.fit(data_train, type)  #训练模型
    result = model.predict(data_test.iloc[:, 1:])   #预测结果
    user_id = data_test.iloc[:, 0]   #将相应结果对应到user_id上，输出
    d = pd.DataFrame({'a':user_id, 'b':result})
    d.to_csv(result_path)
