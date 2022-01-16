import numpy as np
import matplotlib.pyplot as plt
from scipy.linalg import cholesky, cho_solve

class RFF:
	"""
	This class implements Kernel approximation using Random Fourier Features (RFF).
	The notation and references below are to our (potential) AISTATS paper.
	"""

	def __init__(self, X_train, D=100, h=1.0):
		"""
		X : An n x d matrix containing n "raw" input data points. Each row is x_i^T.
		D : Number of RFF samples used for the approximation.
		h : length scale for the Gaussian kernel exp(-|x-y|^2 / (2 * h)).
			The corresponding spectral measure is N(0, 1/h I_d)
		"""
		self.X_train = X_train
		self.D = D
		self.h = h
		self.n, self.d = X_train.shape
		# The matrix containing the D RFF samples. Shape = (D, d).
		self.GAMMA = np.random.multivariate_normal(mean=np.zeros(self.d), cov=(1/h)*np.identity(self.d), size=D)
		# The b's of the paper.
		self.B = np.random.uniform(0, 2*np.pi, size=(self.D, 1))

	def get_Z(self, X):
		"""
		Computes the low rank factorization matrix Z.

		# Input
		X : An m x d matric containing "raw" data points.

		# Returns
		Z : An m x D matrix containing "projected" data points.
		"""
		m, d = X.shape
		assert d == self.d, "The dimensionality of the row of the input matrix is {0}, while the expected is {1}".format(d, self.d)
		Z = np.sqrt(2 / self.D) * np.cos(np.matmul(self.GAMMA, X.T) + self.B)
		Z = Z.T
		return Z

	def get_K_approx(self):
		"""
		Computes an approximation of kernel matrix.
		"""
		Z_train = self.get_Z(self.X_train)
		K_approx = np.matmul(Z_train, Z_train.T)
		return K_approx

	def fit_GP(self, X_train, y_train, sigmasq=-1.0):
		if sigmasq <= 0:
			sigmasq = np.var(y_train) / 8
		n = X_train.shape[0]
		# n x D matrix
		Z = self.get_Z(X_train)
		# n x n matrix.
		Y = np.matmul(Z, Z.T) + sigmasq * np.eye(n)
		# L in Algorithm 2.1 of GPML.
		# It is n x n.
		L = cholesky(Y, lower=True)
		# alpha in Algorithm 2.1 of GPML.
		# It is n x 1.
		alpha = cho_solve((L, True), y_train)
		#alpha_naive = np.matmul(np.linalg.inv(Y), y_train)
		#np.allclose(alpha, alpha_naive)
		self.Z_train = Z
		self.L = L
		self.alpha = alpha
		self.Y = Y

	def predict_GP(self, X_test):
		"""
		Returns the mean and covariance of prediction on test data.
		"""
		m = X_test.shape[0]
		Z_test = self.get_Z(X_test)
		# K_star is an m x n matrix where n = number of rows in Z_train.
		# It corresponds to K(X_star, X) of equation 2.21 in GPML.
		K_star = np.matmul(Z_test, self.Z_train.T)
		# Equation 2.23 in GPML. Mean prediction.
		# It is m x 1
		f_star_bar = np.matmul(K_star, self.alpha)
		# v in Algorithm 2.1 of GPML.
		# It is n x m.
		v = cho_solve((self.L, True), K_star.T)
		#v_naive = np.matmul(np.linalg.inv(self.Y), K_star.T)
		#np.allclose(v, v_naive)
		# K(X_star, X_star) of equation 2.21 in GPML.
		# It is m x m.
		K_star_star = np.matmul(Z_test, Z_test.T)
		# Equation 2.24 of GPML.
		# It is m x m.
		cov_f_star = K_star_star - np.matmul(K_star, v)
		self.f_star_bar = f_star_bar
		self.cov_f_star = cov_f_star
		return f_star_bar, cov_f_star