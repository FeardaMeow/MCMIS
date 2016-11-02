import numpy as np
import math
from sklearn import svm


name_vec = ["dotnumbers", "injuries", "fatalities", "severity"]
def main():
	csv = np.genfromtxt ('crashdata.csv', delimiter=",")
	train_mat = []
	test_mat = []
	count = 0
	l = len(csv)
	train_size = int(l * 0.1)
	test_size = l - train_size

	train_label = [0 for i in range(train_size)]
	test_label = [0 for i in range(test_size)]

	divisor = 500
	for i in range(train_size):
		row = csv[i]
		train_label[count] = int(row[-1] / divisor)
		count += 1
		train_mat.append(row[:-1])

	for i in range(test_size):
		row = csv[train_size + i]
		test_label[i] = int(row[-1] / divisor)
		if test_label[i] > 0:
			print test_label[i]
		count += 1
		test_mat.append(row[:-1])
	train_mat = np.asarray(train_mat)
	test_mat = np.asarray(test_mat)

	clf = svm.LinearSVC()
	clf.fit(train_mat, train_label)
	prediction = clf.predict(test_mat)
	correct = 0
	print test_size
	for i in range(test_size):
		if int(test_label[i]) == int(prediction[i]):
			correct += 1
	print correct
	# print len(prediction)
if __name__ == "__main__":
	main()