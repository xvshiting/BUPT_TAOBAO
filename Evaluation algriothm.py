from __future__ import division
import numpy as np

def getDict(file_name):
	dict = {}
	fp = open(file_name)
	lines = fp.readlines()
	fp.close()
	for line in lines:
		items = line.split(',')
		dict[items[0]] = int(items[1][:-1])
	return dict

def getStat(file_name):
	print file_name
	dict_true = getDict('right.csv')
	dict_false = getDict(file_name)
	num = [0] * 13
	inputNum = [0] * 13
	TP = [0] * 13
	FP = [0] * 13
	FN = [0] * 13
	P = [0] * 13
	R = [0] * 13
	F = [0] * 13
	for key, value in dict_true.items():
		class_true = value
		num[value] += 1
		try:
			class_false = dict_false[key]
			inputNum[class_false] += 1
			if class_true == class_false:
				TP[class_true] += 1
			else:
				FN[class_true] += 1
				FP[class_false] += 1
		except:
			for i in range(1,13):
				if i == class_true:
					FN[i] += 1
				else:
					FP[i] += 1
	
	count = 0
	total = 0
	for i in range(1,13):
		try:
			P[i] = TP[i] / (TP[i] + FP[i])
		except:
			P[i] = 0
		try:
			R[i] = TP[i] / (TP[i] + FN[i])
		except:
			R[i] = 0
		try:
			F[i] = 2 * P[i] * R[i] / (P[i] + R[i])
		except:
			F[i] = 0
		count += num[i]
		total += F[i] * num[i]
	F1 = total / count
	print P[1:13]
	print R[1:13]
	print F[1:13]
	print F1
	print '\n'

getStat('test.txt')
