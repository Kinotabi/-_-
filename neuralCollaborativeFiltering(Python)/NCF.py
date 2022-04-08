import numpy as np
from sklearn.utils import shuffle
from Loader import Loader
from model.NeuMF import NeuMF
from tensorflow.keras.layers import *
from tensorflow.keras.models import Model
import pandas as pd
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import scipy as sp
from sklearn.metrics.pairwise import cosine_similarity
import operator

class cossim_recommender:

    def __init__(self):
        print("Prepare dataset...")
        path = "D:/code/data_project/recommendations/neural_collaborative_filtering-master/Data/grad/ratings.csv"
        df=pd.read_csv(path)
        print("successfully get the dataset")
        #데이터의 크기 출력하기
        df.columns = ['user', 'item', 'rating']
        df = df.dropna()
        df = df.loc[df.rating != 0]
        print(df.shape)
        print(df.info())
        print(df.head())

        # user, item 아이디 부여
        df['user_id'] = df['user'].astype("category").cat.codes
        print(df['user_id'])
        df['item_id'] = df['item'].astype("category").cat.codes
        users = list(np.sort(df.user_id.unique()))
        items = list(np.sort(df.item_id.unique()))
        
        # lookup 테이블 생성
        user_lookup = df[['user', 'user_id']].drop_duplicates()
        user_lookup['user'] = df.user.astype(str)
        user_lookup.to_csv("user_lookup.csv")

        std_class_rating = df.pivot_table('rating', index='item_id', columns='user_id')
        std_class_rating.head()
        std_class_rating.fillna(0,inplace=True)
        std_class_rating.head()

        user_pivot_sparse = sp.sparse.csr_matrix(std_class_rating.values)
        item_similarity = cosine_similarity(user_pivot_sparse)
        user_similarity = cosine_similarity(user_pivot_sparse.T)
        self.std_class_rating = std_class_rating
        self.item_sim_df = pd.DataFrame(item_similarity, index = std_class_rating.index, columns = std_class_rating.index)
        self.user_sim_df = pd.DataFrame(user_similarity, index = std_class_rating.columns, columns = std_class_rating.columns)
        self.user_list = user_lookup

    def recommendation(self, user):
        id = self.user_list['user_id']
        actual_id = self.user_list['user']
        user_dict = dict(zip(actual_id, id))
        user_id = user_dict[user]
        print(user_id)
        if user_id not in self.std_class_rating.columns:
            return ('No data available on user {}'.format(user))

        sim_users = self.user_sim_df.sort_values(by=user_id, ascending=False).index[1:11]
        best = []
        most_common = {}

        for i in sim_users:
            result_sorted = self.std_class_rating.loc[:, i][(self.std_class_rating.loc[:,user_id]==0)].sort_values(ascending=False)
            best.append(result_sorted.index[:10].tolist())
        print(best)
        for i in range(len(best)):
            for j in best[i]:
                if j in most_common:
                    most_common[j] += 1
                else:
                    most_common[j] = 1
        self.sorted_list = sorted(most_common.items(), key=operator.itemgetter(1), reverse=True)
        return self.sorted_list[:10]

class MLP:

    def __init__(self, user_num, item_num):

        # User embedding
        user = Input(shape=(1,), dtype='int32')
        user_embedding = Embedding(user_num, 32, input_length=user.shape[1])(user)
        user_embedding = Flatten()(user_embedding)

        # Item embedding
        item = Input(shape=(1,), dtype='int32')
        item_embedding = Embedding(item_num, 32, input_length=item.shape[1])(item)
        item_embedding = Flatten()(item_embedding)

        # Merge
        concatenated = Concatenate()([user_embedding, item_embedding])
        dropout = Dropout(rate=0.2)(concatenated)

        # Layer1
        layer_1 = Dense(units=64, activation='relu', name='layer1')(dropout)  
        dropout1 = Dropout(rate=0.2, name='dropout1')(layer_1)                
        batch_norm1 = BatchNormalization(name='batch_norm1')(dropout1)        

        # Layer2
        layer_2 = Dense(units=32, activation='relu', name='layer2')(batch_norm1)  
        dropout2 = Dropout(rate=0.2, name='dropout2')(layer_2)                    
        batch_norm2 = BatchNormalization(name='batch_norm2')(dropout2)            
        # Layer3
        layer_3 = Dense(units=16, activation='relu', name='layer3')(batch_norm2)  

        # Layer4
        layer_4 = Dense(units=8, activation='relu', name='layer4')(layer_3)  
        # Output
        output_layer = Dense(1, kernel_initializer='lecun_uniform', name='output_layer')(layer_4)  # (1,1) / h(8,1)초기화

        # Model
        self.model = Model([user, item], output_layer)
        self.model.compile(optimizer='adam', loss='mse')

    def get_model(self):
        model = self.model
        return model

class GMF:

    def __init__(self, user_num, item_num):

        latent_features = 8

        # User embedding
        user = Input(shape=(1,), dtype='int32')
        user_embedding = Embedding(user_num, latent_features, input_length=user.shape[1])(user)
        user_embedding = Flatten()(user_embedding)

        # Item embedding
        item = Input(shape=(1,), dtype='int32')
        item_embedding = Embedding(item_num, latent_features, input_length=item.shape[1])(item)
        item_embedding = Flatten()(item_embedding)

        # Merge
        concatenated = Multiply()([user_embedding, item_embedding])

        # Output
        output_layer = Dense(1, kernel_initializer='lecun_uniform', name='output_layer')(concatenated)

        # Model
        self.model = Model([user, item], output_layer)
        self.model.compile(optimizer='adam', loss='mse')

    def get_model(self):
        model = self.model
        return model

class NCF:

    def __init__(self):

        # data 로드
        loader = Loader()

        print('start data load..')

        num_neg = 4
        uids, iids, self.df_train, self.df_test, \
        self.df_neg, self.users, self.items = loader.load_dataset()
        user_input, item_input, labels = loader.get_train_instances(uids, iids, num_neg, len(self.items))

        print('end data load..')

        # input data 준비
        user_data_shuff, item_data_shuff, label_data_shuff = shuffle(user_input, item_input, labels)
        user_test_shuff, item_test_shuff, label_test_shuff = shuffle(self.df_test.iloc[:,0],self.df_test.iloc[:,1],self.df_test.iloc[:,2])
        self.user_data_shuff = np.array(user_data_shuff).reshape(-1,1)
        self.item_data_shuff = np.array(item_data_shuff).reshape(-1,1)
        self.label_data_shuff = np.array(label_data_shuff).reshape(-1,1)
        self.user_test_shuff = np.array(user_test_shuff).reshape(-1,1)
        self.item_test_shuff = np.array(item_test_shuff).reshape(-1,1)
        self.label_test_shuff = np.array(label_test_shuff).reshape(-1,1)


    def NCF(self):

        nmf = NeuMF(len(self.users), len(self.items))  # Neural Collaborative Filtering
        gmf = GMF(len(self.users), len(self.items))
        mlp = MLP(len(self.users), len(self.items))

        self.model_nmf = nmf.get_model()
        self.model_gmf = gmf.get_model()
        self.model_mlp = mlp.get_model()
        
        print("start validation")
        print("===NMF===")
        history_nmf = self.model_nmf.fit([self.user_data_shuff, self.item_data_shuff], self.label_data_shuff, epochs=20,
                       batch_size=256, verbose=1)
        print("===GMF===")
        history_gmf = self.model_gmf.fit([self.user_data_shuff, self.item_data_shuff], self.label_data_shuff, epochs=20,
                       batch_size=256, verbose=1)
        print("===MLP===")
        history_mlp = self.model_mlp.fit([self.user_data_shuff, self.item_data_shuff], self.label_data_shuff, epochs=20,
                       batch_size=256, verbose=1)
        pd.DataFrame(history_nmf.history).plot(figsize=(8,5))
        pd.DataFrame(history_gmf.history).plot(figsize=(8,5))
        pd.DataFrame(history_mlp.history).plot(figsize=(8,5))
        plt.show()
        return self.model_nmf

if __name__ == '__main__':
   
    cossim = cossim_recommender()
    recommendation = cossim.recommendation('AAFNnBSW1Ar1C6+FyUV3TuF7')
    print(recommendation)
    
    ncf = NCF()
    model = ncf.NCF()

    # user 한 명에 대한 prediction 예시
    item_list_all = pd.read_csv("D:/code/data_project/recommendations/neural_collaborative_filtering-master/Data/grad/list_all.csv")
    user_list_all = pd.read_csv("D:/code/data_project/recommendations/user_lookup.csv")
    id = user_list_all['user_id']
    actual_id = user_list_all['user']
    user_dict = dict(zip(actual_id, id))
    print(user_dict)
    user_candidate_item = item_list_all.to_numpy().reshape(-1, 1)
    
    original_id = 'AAFNnBSW1Ar1C6+FyUV3TuF7'
    user_id = user_dict[original_id]

    user_input = np.full(len(user_candidate_item), user_id).reshape(-1, 1)
    
    predictions = model.predict([user_input, user_candidate_item])
    predictions = predictions.flatten().tolist()
    item_to_pre_score = {item[0]: pre for item, pre in zip(user_candidate_item, predictions)}  # 후보 아이템 별 예측값
    item_to_pre_score = dict(sorted(item_to_pre_score.items(), key=lambda x: x[1], reverse=True))

    recommend_item_lst = list(item_to_pre_score.keys())
    print('recommend:', recommend_item_lst[0:19])