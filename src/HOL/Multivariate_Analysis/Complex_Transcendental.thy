(*  Author: John Harrison
    Ported from "hol_light/Multivariate/transcendentals.ml" by L C Paulson (2015)
*)

section {* Complex Transcendental Functions *}

theory Complex_Transcendental
imports  "~~/src/HOL/Multivariate_Analysis/Complex_Analysis_Basics"
begin

subsection{*The Exponential Function is Differentiable and Continuous*}

lemma complex_differentiable_at_exp: "exp complex_differentiable at z"
  using DERIV_exp complex_differentiable_def by blast

lemma complex_differentiable_within_exp: "exp complex_differentiable (at z within s)"
  by (simp add: complex_differentiable_at_exp complex_differentiable_at_within)

lemma continuous_within_exp:
  fixes z::"'a::{real_normed_field,banach}"
  shows "continuous (at z within s) exp"
by (simp add: continuous_at_imp_continuous_within)

lemma continuous_on_exp:
  fixes s::"'a::{real_normed_field,banach} set"
  shows "continuous_on s exp"
by (simp add: continuous_on_exp continuous_on_id)

lemma holomorphic_on_exp: "exp holomorphic_on s"
  by (simp add: complex_differentiable_within_exp holomorphic_on_def)

subsection{*Euler and de Moivre formulas.*}

text{*The sine series times @{term i}*}
lemma sin_ii_eq: "(\<lambda>n. (ii * sin_coeff n) * z^n) sums (ii * sin z)"
proof -
  have "(\<lambda>n. ii * sin_coeff n *\<^sub>R z^n) sums (ii * sin z)"
    using sin_converges sums_mult by blast
  then show ?thesis
    by (simp add: scaleR_conv_of_real field_simps)
qed

theorem exp_Euler: "exp(ii * z) = cos(z) + ii * sin(z)"
proof -
  have "(\<lambda>n. (cos_coeff n + ii * sin_coeff n) * z^n)
        = (\<lambda>n. (ii * z) ^ n /\<^sub>R (fact n))"
  proof
    fix n
    show "(cos_coeff n + ii * sin_coeff n) * z^n = (ii * z) ^ n /\<^sub>R (fact n)"
      by (auto simp: cos_coeff_def sin_coeff_def scaleR_conv_of_real field_simps elim!: evenE oddE)
  qed
  also have "... sums (exp (ii * z))"
    by (rule exp_converges)
  finally have "(\<lambda>n. (cos_coeff n + ii * sin_coeff n) * z^n) sums (exp (ii * z))" .
  moreover have "(\<lambda>n. (cos_coeff n + ii * sin_coeff n) * z^n) sums (cos z + ii * sin z)"
    using sums_add [OF cos_converges [of z] sin_ii_eq [of z]]
    by (simp add: field_simps scaleR_conv_of_real)
  ultimately show ?thesis
    using sums_unique2 by blast
qed

corollary exp_minus_Euler: "exp(-(ii * z)) = cos(z) - ii * sin(z)"
  using exp_Euler [of "-z"]
  by simp

lemma sin_exp_eq: "sin z = (exp(ii * z) - exp(-(ii * z))) / (2*ii)"
  by (simp add: exp_Euler exp_minus_Euler)

lemma sin_exp_eq': "sin z = ii * (exp(-(ii * z)) - exp(ii * z)) / 2"
  by (simp add: exp_Euler exp_minus_Euler)

lemma cos_exp_eq:  "cos z = (exp(ii * z) + exp(-(ii * z))) / 2"
  by (simp add: exp_Euler exp_minus_Euler)

subsection{*Relationships between real and complex trig functions*}

lemma real_sin_eq [simp]:
  fixes x::real
  shows "Re(sin(of_real x)) = sin x"
  by (simp add: sin_of_real)

lemma real_cos_eq [simp]:
  fixes x::real
  shows "Re(cos(of_real x)) = cos x"
  by (simp add: cos_of_real)

lemma DeMoivre: "(cos z + ii * sin z) ^ n = cos(n * z) + ii * sin(n * z)"
  apply (simp add: exp_Euler [symmetric])
  by (metis exp_of_nat_mult mult.left_commute)

lemma exp_cnj:
  fixes z::complex
  shows "cnj (exp z) = exp (cnj z)"
proof -
  have "(\<lambda>n. cnj (z ^ n /\<^sub>R (fact n))) = (\<lambda>n. (cnj z)^n /\<^sub>R (fact n))"
    by auto
  also have "... sums (exp (cnj z))"
    by (rule exp_converges)
  finally have "(\<lambda>n. cnj (z ^ n /\<^sub>R (fact n))) sums (exp (cnj z))" .
  moreover have "(\<lambda>n. cnj (z ^ n /\<^sub>R (fact n))) sums (cnj (exp z))"
    by (metis exp_converges sums_cnj)
  ultimately show ?thesis
    using sums_unique2
    by blast
qed

lemma cnj_sin: "cnj(sin z) = sin(cnj z)"
  by (simp add: sin_exp_eq exp_cnj field_simps)

lemma cnj_cos: "cnj(cos z) = cos(cnj z)"
  by (simp add: cos_exp_eq exp_cnj field_simps)

lemma complex_differentiable_at_sin: "sin complex_differentiable at z"
  using DERIV_sin complex_differentiable_def by blast

lemma complex_differentiable_within_sin: "sin complex_differentiable (at z within s)"
  by (simp add: complex_differentiable_at_sin complex_differentiable_at_within)

lemma complex_differentiable_at_cos: "cos complex_differentiable at z"
  using DERIV_cos complex_differentiable_def by blast

lemma complex_differentiable_within_cos: "cos complex_differentiable (at z within s)"
  by (simp add: complex_differentiable_at_cos complex_differentiable_at_within)

lemma holomorphic_on_sin: "sin holomorphic_on s"
  by (simp add: complex_differentiable_within_sin holomorphic_on_def)

lemma holomorphic_on_cos: "cos holomorphic_on s"
  by (simp add: complex_differentiable_within_cos holomorphic_on_def)

subsection{* Get a nice real/imaginary separation in Euler's formula.*}

lemma Euler: "exp(z) = of_real(exp(Re z)) *
              (of_real(cos(Im z)) + ii * of_real(sin(Im z)))"
by (cases z) (simp add: exp_add exp_Euler cos_of_real exp_of_real sin_of_real)

lemma Re_sin: "Re(sin z) = sin(Re z) * (exp(Im z) + exp(-(Im z))) / 2"
  by (simp add: sin_exp_eq field_simps Re_divide Im_exp)

lemma Im_sin: "Im(sin z) = cos(Re z) * (exp(Im z) - exp(-(Im z))) / 2"
  by (simp add: sin_exp_eq field_simps Im_divide Re_exp)

lemma Re_cos: "Re(cos z) = cos(Re z) * (exp(Im z) + exp(-(Im z))) / 2"
  by (simp add: cos_exp_eq field_simps Re_divide Re_exp)

lemma Im_cos: "Im(cos z) = sin(Re z) * (exp(-(Im z)) - exp(Im z)) / 2"
  by (simp add: cos_exp_eq field_simps Im_divide Im_exp)

lemma Re_sin_pos: "0 < Re z \<Longrightarrow> Re z < pi \<Longrightarrow> Re (sin z) > 0"
  by (auto simp: Re_sin Im_sin add_pos_pos sin_gt_zero)

lemma Im_sin_nonneg: "Re z = 0 \<Longrightarrow> 0 \<le> Im z \<Longrightarrow> 0 \<le> Im (sin z)"
  by (simp add: Re_sin Im_sin algebra_simps)

lemma Im_sin_nonneg2: "Re z = pi \<Longrightarrow> Im z \<le> 0 \<Longrightarrow> 0 \<le> Im (sin z)"
  by (simp add: Re_sin Im_sin algebra_simps)

subsection{*More on the Polar Representation of Complex Numbers*}

lemma exp_Complex: "exp(Complex r t) = of_real(exp r) * Complex (cos t) (sin t)"
  by (simp add: exp_add exp_Euler exp_of_real sin_of_real cos_of_real)

lemma exp_eq_1: "exp z = 1 \<longleftrightarrow> Re(z) = 0 \<and> (\<exists>n::int. Im(z) = of_int (2 * n) * pi)"
apply auto
apply (metis exp_eq_one_iff norm_exp_eq_Re norm_one)
apply (metis Re_exp cos_one_2pi_int mult.commute mult.left_neutral norm_exp_eq_Re norm_one one_complex.simps(1) real_of_int_def)
by (metis Im_exp Re_exp complex_Re_Im_cancel_iff cos_one_2pi_int sin_double Re_complex_of_real complex_Re_numeral exp_zero mult.assoc mult.left_commute mult_eq_0_iff mult_numeral_1 numeral_One of_real_0 real_of_int_def sin_zero_iff_int2)

lemma exp_eq: "exp w = exp z \<longleftrightarrow> (\<exists>n::int. w = z + (of_int (2 * n) * pi) * ii)"
                (is "?lhs = ?rhs")
proof -
  have "exp w = exp z \<longleftrightarrow> exp (w-z) = 1"
    by (simp add: exp_diff)
  also have "... \<longleftrightarrow> (Re w = Re z \<and> (\<exists>n::int. Im w - Im z = of_int (2 * n) * pi))"
    by (simp add: exp_eq_1)
  also have "... \<longleftrightarrow> ?rhs"
    by (auto simp: algebra_simps intro!: complex_eqI)
  finally show ?thesis .
qed

lemma exp_complex_eqI: "abs(Im w - Im z) < 2*pi \<Longrightarrow> exp w = exp z \<Longrightarrow> w = z"
  by (auto simp: exp_eq abs_mult)

lemma exp_integer_2pi:
  assumes "n \<in> Ints"
  shows "exp((2 * n * pi) * ii) = 1"
proof -
  have "exp((2 * n * pi) * ii) = exp 0"
    using assms
    by (simp only: Ints_def exp_eq) auto
  also have "... = 1"
    by simp
  finally show ?thesis .
qed

lemma sin_cos_eq_iff: "sin y = sin x \<and> cos y = cos x \<longleftrightarrow> (\<exists>n::int. y = x + 2 * n * pi)"
proof -
  { assume "sin y = sin x" "cos y = cos x"
    then have "cos (y-x) = 1"
      using cos_add [of y "-x"] by simp
    then have "\<exists>n::int. y-x = real n * 2 * pi"
      using cos_one_2pi_int by blast }
  then show ?thesis
  apply (auto simp: sin_add cos_add)
  apply (metis add.commute diff_add_cancel mult.commute)
  done
qed

lemma exp_i_ne_1:
  assumes "0 < x" "x < 2*pi"
  shows "exp(\<i> * of_real x) \<noteq> 1"
proof
  assume "exp (\<i> * of_real x) = 1"
  then have "exp (\<i> * of_real x) = exp 0"
    by simp
  then obtain n where "\<i> * of_real x = (of_int (2 * n) * pi) * \<i>"
    by (simp only: Ints_def exp_eq) auto
  then have  "of_real x = (of_int (2 * n) * pi)"
    by (metis complex_i_not_zero mult.commute mult_cancel_left of_real_eq_iff real_scaleR_def scaleR_conv_of_real)
  then have  "x = (of_int (2 * n) * pi)"
    by simp
  then show False using assms
    by (cases n) (auto simp: zero_less_mult_iff mult_less_0_iff)
qed

lemma sin_eq_0:
  fixes z::complex
  shows "sin z = 0 \<longleftrightarrow> (\<exists>n::int. z = of_real(n * pi))"
  by (simp add: sin_exp_eq exp_eq of_real_numeral)

lemma cos_eq_0:
  fixes z::complex
  shows "cos z = 0 \<longleftrightarrow> (\<exists>n::int. z = of_real(n * pi) + of_real pi/2)"
  using sin_eq_0 [of "z - of_real pi/2"]
  by (simp add: sin_diff algebra_simps)

lemma cos_eq_1:
  fixes z::complex
  shows "cos z = 1 \<longleftrightarrow> (\<exists>n::int. z = of_real(2 * n * pi))"
proof -
  have "cos z = cos (2*(z/2))"
    by simp
  also have "... = 1 - 2 * sin (z/2) ^ 2"
    by (simp only: cos_double_sin)
  finally have [simp]: "cos z = 1 \<longleftrightarrow> sin (z/2) = 0"
    by simp
  show ?thesis
    by (auto simp: sin_eq_0 of_real_numeral)
qed

lemma csin_eq_1:
  fixes z::complex
  shows "sin z = 1 \<longleftrightarrow> (\<exists>n::int. z = of_real(2 * n * pi) + of_real pi/2)"
  using cos_eq_1 [of "z - of_real pi/2"]
  by (simp add: cos_diff algebra_simps)

lemma csin_eq_minus1:
  fixes z::complex
  shows "sin z = -1 \<longleftrightarrow> (\<exists>n::int. z = of_real(2 * n * pi) + 3/2*pi)"
        (is "_ = ?rhs")
proof -
  have "sin z = -1 \<longleftrightarrow> sin (-z) = 1"
    by (simp add: equation_minus_iff)
  also have "...  \<longleftrightarrow> (\<exists>n::int. -z = of_real(2 * n * pi) + of_real pi/2)"
    by (simp only: csin_eq_1)
  also have "...  \<longleftrightarrow> (\<exists>n::int. z = - of_real(2 * n * pi) - of_real pi/2)"
    apply (rule iff_exI)
    by (metis (no_types)  is_num_normalize(8) minus_minus of_real_def real_vector.scale_minus_left uminus_add_conv_diff)
  also have "... = ?rhs"
    apply (auto simp: of_real_numeral)
    apply (rule_tac [2] x="-(x+1)" in exI)
    apply (rule_tac x="-(x+1)" in exI)
    apply (simp_all add: algebra_simps)
    done
  finally show ?thesis .
qed

lemma ccos_eq_minus1:
  fixes z::complex
  shows "cos z = -1 \<longleftrightarrow> (\<exists>n::int. z = of_real(2 * n * pi) + pi)"
  using csin_eq_1 [of "z - of_real pi/2"]
  apply (simp add: sin_diff)
  apply (simp add: algebra_simps of_real_numeral equation_minus_iff)
  done

lemma sin_eq_1: "sin x = 1 \<longleftrightarrow> (\<exists>n::int. x = (2 * n + 1 / 2) * pi)"
                (is "_ = ?rhs")
proof -
  have "sin x = 1 \<longleftrightarrow> sin (complex_of_real x) = 1"
    by (metis of_real_1 one_complex.simps(1) real_sin_eq sin_of_real)
  also have "...  \<longleftrightarrow> (\<exists>n::int. complex_of_real x = of_real(2 * n * pi) + of_real pi/2)"
    by (simp only: csin_eq_1)
  also have "...  \<longleftrightarrow> (\<exists>n::int. x = of_real(2 * n * pi) + of_real pi/2)"
    apply (rule iff_exI)
    apply (auto simp: algebra_simps of_real_numeral)
    apply (rule injD [OF inj_of_real [where 'a = complex]])
    apply (auto simp: of_real_numeral)
    done
  also have "... = ?rhs"
    by (auto simp: algebra_simps)
  finally show ?thesis .
qed

lemma sin_eq_minus1: "sin x = -1 \<longleftrightarrow> (\<exists>n::int. x = (2*n + 3/2) * pi)"  (is "_ = ?rhs")
proof -
  have "sin x = -1 \<longleftrightarrow> sin (complex_of_real x) = -1"
    by (metis Re_complex_of_real of_real_def scaleR_minus1_left sin_of_real)
  also have "...  \<longleftrightarrow> (\<exists>n::int. complex_of_real x = of_real(2 * n * pi) + 3/2*pi)"
    by (simp only: csin_eq_minus1)
  also have "...  \<longleftrightarrow> (\<exists>n::int. x = of_real(2 * n * pi) + 3/2*pi)"
    apply (rule iff_exI)
    apply (auto simp: algebra_simps)
    apply (rule injD [OF inj_of_real [where 'a = complex]], auto)
    done
  also have "... = ?rhs"
    by (auto simp: algebra_simps)
  finally show ?thesis .
qed

lemma cos_eq_minus1: "cos x = -1 \<longleftrightarrow> (\<exists>n::int. x = (2*n + 1) * pi)"
                      (is "_ = ?rhs")
proof -
  have "cos x = -1 \<longleftrightarrow> cos (complex_of_real x) = -1"
    by (metis Re_complex_of_real of_real_def scaleR_minus1_left cos_of_real)
  also have "...  \<longleftrightarrow> (\<exists>n::int. complex_of_real x = of_real(2 * n * pi) + pi)"
    by (simp only: ccos_eq_minus1)
  also have "...  \<longleftrightarrow> (\<exists>n::int. x = of_real(2 * n * pi) + pi)"
    apply (rule iff_exI)
    apply (auto simp: algebra_simps)
    apply (rule injD [OF inj_of_real [where 'a = complex]], auto)
    done
  also have "... = ?rhs"
    by (auto simp: algebra_simps)
  finally show ?thesis .
qed

lemma dist_exp_ii_1: "norm(exp(ii * of_real t) - 1) = 2 * abs(sin(t / 2))"
  apply (simp add: exp_Euler cmod_def power2_diff sin_of_real cos_of_real algebra_simps)
  using cos_double_sin [of "t/2"]
  apply (simp add: real_sqrt_mult)
  done

lemma sinh_complex:
  fixes z :: complex
  shows "(exp z - inverse (exp z)) / 2 = -ii * sin(ii * z)"
  by (simp add: sin_exp_eq divide_simps exp_minus of_real_numeral)

lemma sin_ii_times:
  fixes z :: complex
  shows "sin(ii * z) = ii * ((exp z - inverse (exp z)) / 2)"
  using sinh_complex by auto

lemma sinh_real:
  fixes x :: real
  shows "of_real((exp x - inverse (exp x)) / 2) = -ii * sin(ii * of_real x)"
  by (simp add: exp_of_real sin_ii_times of_real_numeral)

lemma cosh_complex:
  fixes z :: complex
  shows "(exp z + inverse (exp z)) / 2 = cos(ii * z)"
  by (simp add: cos_exp_eq divide_simps exp_minus of_real_numeral exp_of_real)

lemma cosh_real:
  fixes x :: real
  shows "of_real((exp x + inverse (exp x)) / 2) = cos(ii * of_real x)"
  by (simp add: cos_exp_eq divide_simps exp_minus of_real_numeral exp_of_real)

lemmas cos_ii_times = cosh_complex [symmetric]

lemma norm_cos_squared:
    "norm(cos z) ^ 2 = cos(Re z) ^ 2 + (exp(Im z) - inverse(exp(Im z))) ^ 2 / 4"
  apply (cases z)
  apply (simp add: cos_add cmod_power2 cos_of_real sin_of_real)
  apply (simp add: cos_exp_eq sin_exp_eq exp_minus exp_of_real Re_divide Im_divide)
  apply (simp only: left_diff_distrib [symmetric] power_mult_distrib)
  apply (simp add: sin_squared_eq)
  apply (simp add: power2_eq_square algebra_simps divide_simps)
  done

lemma norm_sin_squared:
    "norm(sin z) ^ 2 = (exp(2 * Im z) + inverse(exp(2 * Im z)) - 2 * cos(2 * Re z)) / 4"
  apply (cases z)
  apply (simp add: sin_add cmod_power2 cos_of_real sin_of_real cos_double_cos exp_double)
  apply (simp add: cos_exp_eq sin_exp_eq exp_minus exp_of_real Re_divide Im_divide)
  apply (simp only: left_diff_distrib [symmetric] power_mult_distrib)
  apply (simp add: cos_squared_eq)
  apply (simp add: power2_eq_square algebra_simps divide_simps)
  done

lemma exp_uminus_Im: "exp (- Im z) \<le> exp (cmod z)"
  using abs_Im_le_cmod linear order_trans by fastforce

lemma norm_cos_le:
  fixes z::complex
  shows "norm(cos z) \<le> exp(norm z)"
proof -
  have "Im z \<le> cmod z"
    using abs_Im_le_cmod abs_le_D1 by auto
  with exp_uminus_Im show ?thesis
    apply (simp add: cos_exp_eq norm_divide)
    apply (rule order_trans [OF norm_triangle_ineq], simp)
    apply (metis add_mono exp_le_cancel_iff mult_2_right)
    done
qed

lemma norm_cos_plus1_le:
  fixes z::complex
  shows "norm(1 + cos z) \<le> 2 * exp(norm z)"
proof -
  have mono: "\<And>u w z::real. (1 \<le> w | 1 \<le> z) \<Longrightarrow> (w \<le> u & z \<le> u) \<Longrightarrow> 2 + w + z \<le> 4 * u"
      by arith
  have *: "Im z \<le> cmod z"
    using abs_Im_le_cmod abs_le_D1 by auto
  have triangle3: "\<And>x y z. norm(x + y + z) \<le> norm(x) + norm(y) + norm(z)"
    by (simp add: norm_add_rule_thm)
  have "norm(1 + cos z) = cmod (1 + (exp (\<i> * z) + exp (- (\<i> * z))) / 2)"
    by (simp add: cos_exp_eq)
  also have "... = cmod ((2 + exp (\<i> * z) + exp (- (\<i> * z))) / 2)"
    by (simp add: field_simps)
  also have "... = cmod (2 + exp (\<i> * z) + exp (- (\<i> * z))) / 2"
    by (simp add: norm_divide)
  finally show ?thesis
    apply (rule ssubst, simp)
    apply (rule order_trans [OF triangle3], simp)
    using exp_uminus_Im *
    apply (auto intro: mono)
    done
qed

subsection{* Taylor series for complex exponential, sine and cosine.*}

context
begin

declare power_Suc [simp del]

lemma Taylor_exp:
  "norm(exp z - (\<Sum>k\<le>n. z ^ k / (fact k))) \<le> exp\<bar>Re z\<bar> * (norm z) ^ (Suc n) / (fact n)"
proof (rule complex_taylor [of _ n "\<lambda>k. exp" "exp\<bar>Re z\<bar>" 0 z, simplified])
  show "convex (closed_segment 0 z)"
    by (rule convex_segment [of 0 z])
next
  fix k x
  assume "x \<in> closed_segment 0 z" "k \<le> n"
  show "(exp has_field_derivative exp x) (at x within closed_segment 0 z)"
    using DERIV_exp DERIV_subset by blast
next
  fix x
  assume "x \<in> closed_segment 0 z"
  then show "Re x \<le> \<bar>Re z\<bar>"
    apply (auto simp: closed_segment_def scaleR_conv_of_real)
    by (meson abs_ge_self abs_ge_zero linear mult_left_le_one_le mult_nonneg_nonpos order_trans)
next
  show "0 \<in> closed_segment 0 z"
    by (auto simp: closed_segment_def)
next
  show "z \<in> closed_segment 0 z"
    apply (simp add: closed_segment_def scaleR_conv_of_real)
    using of_real_1 zero_le_one by blast
qed

lemma
  assumes "0 \<le> u" "u \<le> 1"
  shows cmod_sin_le_exp: "cmod (sin (u *\<^sub>R z)) \<le> exp \<bar>Im z\<bar>"
    and cmod_cos_le_exp: "cmod (cos (u *\<^sub>R z)) \<le> exp \<bar>Im z\<bar>"
proof -
  have mono: "\<And>u w z::real. w \<le> u \<Longrightarrow> z \<le> u \<Longrightarrow> w + z \<le> u*2"
    by arith
  show "cmod (sin (u *\<^sub>R z)) \<le> exp \<bar>Im z\<bar>" using assms
    apply (auto simp: scaleR_conv_of_real norm_mult norm_power sin_exp_eq norm_divide)
    apply (rule order_trans [OF norm_triangle_ineq4])
    apply (rule mono)
    apply (auto simp: abs_if mult_left_le_one_le)
    apply (meson mult_nonneg_nonneg neg_le_0_iff_le not_le order_trans)
    apply (meson less_eq_real_def mult_nonneg_nonpos neg_0_le_iff_le order_trans)
    done
  show "cmod (cos (u *\<^sub>R z)) \<le> exp \<bar>Im z\<bar>" using assms
    apply (auto simp: scaleR_conv_of_real norm_mult norm_power cos_exp_eq norm_divide)
    apply (rule order_trans [OF norm_triangle_ineq])
    apply (rule mono)
    apply (auto simp: abs_if mult_left_le_one_le)
    apply (meson mult_nonneg_nonneg neg_le_0_iff_le not_le order_trans)
    apply (meson less_eq_real_def mult_nonneg_nonpos neg_0_le_iff_le order_trans)
    done
qed

lemma Taylor_sin:
  "norm(sin z - (\<Sum>k\<le>n. complex_of_real (sin_coeff k) * z ^ k))
   \<le> exp\<bar>Im z\<bar> * (norm z) ^ (Suc n) / (fact n)"
proof -
  have mono: "\<And>u w z::real. w \<le> u \<Longrightarrow> z \<le> u \<Longrightarrow> w + z \<le> u*2"
      by arith
  have *: "cmod (sin z -
                 (\<Sum>i\<le>n. (-1) ^ (i div 2) * (if even i then sin 0 else cos 0) * z ^ i / (fact i)))
           \<le> exp \<bar>Im z\<bar> * cmod z ^ Suc n / (fact n)"
  proof (rule complex_taylor [of "closed_segment 0 z" n "\<lambda>k x. (-1)^(k div 2) * (if even k then sin x else cos x)" "exp\<bar>Im z\<bar>" 0 z,
simplified])
  show "convex (closed_segment 0 z)"
    by (rule convex_segment [of 0 z])
  next
    fix k x
    show "((\<lambda>x. (- 1) ^ (k div 2) * (if even k then sin x else cos x)) has_field_derivative
            (- 1) ^ (Suc k div 2) * (if odd k then sin x else cos x))
            (at x within closed_segment 0 z)"
      apply (auto simp: power_Suc)
      apply (intro derivative_eq_intros | simp)+
      done
  next
    fix x
    assume "x \<in> closed_segment 0 z"
    then show "cmod ((- 1) ^ (Suc n div 2) * (if odd n then sin x else cos x)) \<le> exp \<bar>Im z\<bar>"
      by (auto simp: closed_segment_def norm_mult norm_power cmod_sin_le_exp cmod_cos_le_exp)
  next
    show "0 \<in> closed_segment 0 z"
      by (auto simp: closed_segment_def)
  next
    show "z \<in> closed_segment 0 z"
      apply (simp add: closed_segment_def scaleR_conv_of_real)
      using of_real_1 zero_le_one by blast
  qed
  have **: "\<And>k. complex_of_real (sin_coeff k) * z ^ k
            = (-1)^(k div 2) * (if even k then sin 0 else cos 0) * z^k / of_nat (fact k)"
    by (auto simp: sin_coeff_def elim!: oddE)
  show ?thesis
    apply (rule order_trans [OF _ *])
    apply (simp add: **)
    done
qed

lemma Taylor_cos:
  "norm(cos z - (\<Sum>k\<le>n. complex_of_real (cos_coeff k) * z ^ k))
   \<le> exp\<bar>Im z\<bar> * (norm z) ^ Suc n / (fact n)"
proof -
  have mono: "\<And>u w z::real. w \<le> u \<Longrightarrow> z \<le> u \<Longrightarrow> w + z \<le> u*2"
      by arith
  have *: "cmod (cos z -
                 (\<Sum>i\<le>n. (-1) ^ (Suc i div 2) * (if even i then cos 0 else sin 0) * z ^ i / (fact i)))
           \<le> exp \<bar>Im z\<bar> * cmod z ^ Suc n / (fact n)"
  proof (rule complex_taylor [of "closed_segment 0 z" n "\<lambda>k x. (-1)^(Suc k div 2) * (if even k then cos x else sin x)" "exp\<bar>Im z\<bar>" 0 z,
simplified])
  show "convex (closed_segment 0 z)"
    by (rule convex_segment [of 0 z])
  next
    fix k x
    assume "x \<in> closed_segment 0 z" "k \<le> n"
    show "((\<lambda>x. (- 1) ^ (Suc k div 2) * (if even k then cos x else sin x)) has_field_derivative
            (- 1) ^ Suc (k div 2) * (if odd k then cos x else sin x))
             (at x within closed_segment 0 z)"
      apply (auto simp: power_Suc)
      apply (intro derivative_eq_intros | simp)+
      done
  next
    fix x
    assume "x \<in> closed_segment 0 z"
    then show "cmod ((- 1) ^ Suc (n div 2) * (if odd n then cos x else sin x)) \<le> exp \<bar>Im z\<bar>"
      by (auto simp: closed_segment_def norm_mult norm_power cmod_sin_le_exp cmod_cos_le_exp)
  next
    show "0 \<in> closed_segment 0 z"
      by (auto simp: closed_segment_def)
  next
    show "z \<in> closed_segment 0 z"
      apply (simp add: closed_segment_def scaleR_conv_of_real)
      using of_real_1 zero_le_one by blast
  qed
  have **: "\<And>k. complex_of_real (cos_coeff k) * z ^ k
            = (-1)^(Suc k div 2) * (if even k then cos 0 else sin 0) * z^k / of_nat (fact k)"
    by (auto simp: cos_coeff_def elim!: evenE)
  show ?thesis
    apply (rule order_trans [OF _ *])
    apply (simp add: **)
    done
qed

end (* of context *)

text{*32-bit Approximation to e*}
lemma e_approx_32: "abs(exp(1) - 5837465777 / 2147483648) \<le> (inverse(2 ^ 32)::real)"
  using Taylor_exp [of 1 14] exp_le
  apply (simp add: setsum_left_distrib in_Reals_norm Re_exp atMost_nat_numeral fact_numeral)
  apply (simp only: pos_le_divide_eq [symmetric], linarith)
  done

subsection{*The argument of a complex number*}

definition Arg :: "complex \<Rightarrow> real" where
 "Arg z \<equiv> if z = 0 then 0
           else THE t. 0 \<le> t \<and> t < 2*pi \<and>
                    z = of_real(norm z) * exp(ii * of_real t)"

lemma Arg_0 [simp]: "Arg(0) = 0"
  by (simp add: Arg_def)

lemma Arg_unique_lemma:
  assumes z:  "z = of_real(norm z) * exp(ii * of_real t)"
      and z': "z = of_real(norm z) * exp(ii * of_real t')"
      and t:  "0 \<le> t"  "t < 2*pi"
      and t': "0 \<le> t'" "t' < 2*pi"
      and nz: "z \<noteq> 0"
  shows "t' = t"
proof -
  have [dest]: "\<And>x y z::real. x\<ge>0 \<Longrightarrow> x+y < z \<Longrightarrow> y<z"
    by arith
  have "of_real (cmod z) * exp (\<i> * of_real t') = of_real (cmod z) * exp (\<i> * of_real t)"
    by (metis z z')
  then have "exp (\<i> * of_real t') = exp (\<i> * of_real t)"
    by (metis nz mult_left_cancel mult_zero_left z)
  then have "sin t' = sin t \<and> cos t' = cos t"
    apply (simp add: exp_Euler sin_of_real cos_of_real)
    by (metis Complex_eq complex.sel)
  then obtain n::int where n: "t' = t + 2 * real n * pi"
    by (auto simp: sin_cos_eq_iff)
  then have "n=0"
    apply (rule_tac z=n in int_cases)
    using t t'
    apply (auto simp: mult_less_0_iff algebra_simps)
    done
  then show "t' = t"
      by (simp add: n)
qed

lemma Arg: "0 \<le> Arg z & Arg z < 2*pi & z = of_real(norm z) * exp(ii * of_real(Arg z))"
proof (cases "z=0")
  case True then show ?thesis
    by (simp add: Arg_def)
next
  case False
  obtain t where t: "0 \<le> t" "t < 2*pi"
             and ReIm: "Re z / cmod z = cos t" "Im z / cmod z = sin t"
    using sincos_total_2pi [OF complex_unit_circle [OF False]]
    by blast
  have z: "z = of_real(norm z) * exp(ii * of_real t)"
    apply (rule complex_eqI)
    using t False ReIm
    apply (auto simp: exp_Euler sin_of_real cos_of_real divide_simps)
    done
  show ?thesis
    apply (simp add: Arg_def False)
    apply (rule theI [where a=t])
    using t z False
    apply (auto intro: Arg_unique_lemma)
    done
qed


corollary
  shows Arg_ge_0: "0 \<le> Arg z"
    and Arg_lt_2pi: "Arg z < 2*pi"
    and Arg_eq: "z = of_real(norm z) * exp(ii * of_real(Arg z))"
  using Arg by auto

lemma complex_norm_eq_1_exp: "norm z = 1 \<longleftrightarrow> (\<exists>t. z = exp(ii * of_real t))"
  using Arg [of z] by auto

lemma Arg_unique: "\<lbrakk>of_real r * exp(ii * of_real a) = z; 0 < r; 0 \<le> a; a < 2*pi\<rbrakk> \<Longrightarrow> Arg z = a"
  apply (rule Arg_unique_lemma [OF _ Arg_eq])
  using Arg [of z]
  apply (auto simp: norm_mult)
  done

lemma Arg_minus: "z \<noteq> 0 \<Longrightarrow> Arg (-z) = (if Arg z < pi then Arg z + pi else Arg z - pi)"
  apply (rule Arg_unique [of "norm z"])
  apply (rule complex_eqI)
  using Arg_ge_0 [of z] Arg_eq [of z] Arg_lt_2pi [of z] Arg_eq [of z]
  apply auto
  apply (auto simp: Re_exp Im_exp cos_diff sin_diff cis_conv_exp [symmetric])
  apply (metis Re_rcis Im_rcis rcis_def)+
  done

lemma Arg_times_of_real [simp]: "0 < r \<Longrightarrow> Arg (of_real r * z) = Arg z"
  apply (cases "z=0", simp)
  apply (rule Arg_unique [of "r * norm z"])
  using Arg
  apply auto
  done

lemma Arg_times_of_real2 [simp]: "0 < r \<Longrightarrow> Arg (z * of_real r) = Arg z"
  by (metis Arg_times_of_real mult.commute)

lemma Arg_divide_of_real [simp]: "0 < r \<Longrightarrow> Arg (z / of_real r) = Arg z"
  by (metis Arg_times_of_real2 less_numeral_extra(3) nonzero_eq_divide_eq of_real_eq_0_iff)

lemma Arg_le_pi: "Arg z \<le> pi \<longleftrightarrow> 0 \<le> Im z"
proof (cases "z=0")
  case True then show ?thesis
    by simp
next
  case False
  have "0 \<le> Im z \<longleftrightarrow> 0 \<le> Im (of_real (cmod z) * exp (\<i> * complex_of_real (Arg z)))"
    by (metis Arg_eq)
  also have "... = (0 \<le> Im (exp (\<i> * complex_of_real (Arg z))))"
    using False
    by (simp add: zero_le_mult_iff)
  also have "... \<longleftrightarrow> Arg z \<le> pi"
    by (simp add: Im_exp) (metis Arg_ge_0 Arg_lt_2pi sin_lt_zero sin_ge_zero not_le)
  finally show ?thesis
    by blast
qed

lemma Arg_lt_pi: "0 < Arg z \<and> Arg z < pi \<longleftrightarrow> 0 < Im z"
proof (cases "z=0")
  case True then show ?thesis
    by simp
next
  case False
  have "0 < Im z \<longleftrightarrow> 0 < Im (of_real (cmod z) * exp (\<i> * complex_of_real (Arg z)))"
    by (metis Arg_eq)
  also have "... = (0 < Im (exp (\<i> * complex_of_real (Arg z))))"
    using False
    by (simp add: zero_less_mult_iff)
  also have "... \<longleftrightarrow> 0 < Arg z \<and> Arg z < pi"
    using Arg_ge_0  Arg_lt_2pi sin_le_zero sin_gt_zero
    apply (auto simp: Im_exp)
    using le_less apply fastforce
    using not_le by blast
  finally show ?thesis
    by blast
qed

lemma Arg_eq_0: "Arg z = 0 \<longleftrightarrow> z \<in> Reals \<and> 0 \<le> Re z"
proof (cases "z=0")
  case True then show ?thesis
    by simp
next
  case False
  have "z \<in> Reals \<and> 0 \<le> Re z \<longleftrightarrow> z \<in> Reals \<and> 0 \<le> Re (of_real (cmod z) * exp (\<i> * complex_of_real (Arg z)))"
    by (metis Arg_eq)
  also have "... \<longleftrightarrow> z \<in> Reals \<and> 0 \<le> Re (exp (\<i> * complex_of_real (Arg z)))"
    using False
    by (simp add: zero_le_mult_iff)
  also have "... \<longleftrightarrow> Arg z = 0"
    apply (auto simp: Re_exp)
    apply (metis Arg_lt_pi Arg_ge_0 Arg_le_pi cos_pi complex_is_Real_iff leD less_linear less_minus_one_simps(2) minus_minus neg_less_eq_nonneg order_refl)
    using Arg_eq [of z]
    apply (auto simp: Reals_def)
    done
  finally show ?thesis
    by blast
qed

lemma Arg_of_real: "Arg(of_real x) = 0 \<longleftrightarrow> 0 \<le> x"
  by (simp add: Arg_eq_0)

lemma Arg_eq_pi: "Arg z = pi \<longleftrightarrow> z \<in> \<real> \<and> Re z < 0"
  apply  (cases "z=0", simp)
  using Arg_eq_0 [of "-z"]
  apply (auto simp: complex_is_Real_iff Arg_minus)
  apply (simp add: complex_Re_Im_cancel_iff)
  apply (metis Arg_minus pi_gt_zero add.left_neutral minus_minus minus_zero)
  done

lemma Arg_eq_0_pi: "Arg z = 0 \<or> Arg z = pi \<longleftrightarrow> z \<in> \<real>"
  using Arg_eq_0 Arg_eq_pi not_le by auto

lemma Arg_inverse: "Arg(inverse z) = (if z \<in> \<real> \<and> 0 \<le> Re z then Arg z else 2*pi - Arg z)"
  apply (cases "z=0", simp)
  apply (rule Arg_unique [of "inverse (norm z)"])
  using Arg_ge_0 [of z] Arg_lt_2pi [of z] Arg_eq [of z] Arg_eq_0 [of z] Exp_two_pi_i
  apply (auto simp: of_real_numeral algebra_simps exp_diff divide_simps)
  done

lemma Arg_eq_iff:
  assumes "w \<noteq> 0" "z \<noteq> 0"
     shows "Arg w = Arg z \<longleftrightarrow> (\<exists>x. 0 < x & w = of_real x * z)"
  using assms Arg_eq [of z] Arg_eq [of w]
  apply auto
  apply (rule_tac x="norm w / norm z" in exI)
  apply (simp add: divide_simps)
  by (metis mult.commute mult.left_commute)

lemma Arg_inverse_eq_0: "Arg(inverse z) = 0 \<longleftrightarrow> Arg z = 0"
  using complex_is_Real_iff
  apply (simp add: Arg_eq_0)
  apply (auto simp: divide_simps not_sum_power2_lt_zero)
  done

lemma Arg_divide:
  assumes "w \<noteq> 0" "z \<noteq> 0" "Arg w \<le> Arg z"
    shows "Arg(z / w) = Arg z - Arg w"
  apply (rule Arg_unique [of "norm(z / w)"])
  using assms Arg_eq [of z] Arg_eq [of w] Arg_ge_0 [of w] Arg_lt_2pi [of z]
  apply (auto simp: exp_diff norm_divide algebra_simps divide_simps)
  done

lemma Arg_le_div_sum:
  assumes "w \<noteq> 0" "z \<noteq> 0" "Arg w \<le> Arg z"
    shows "Arg z = Arg w + Arg(z / w)"
  by (simp add: Arg_divide assms)

lemma Arg_le_div_sum_eq:
  assumes "w \<noteq> 0" "z \<noteq> 0"
    shows "Arg w \<le> Arg z \<longleftrightarrow> Arg z = Arg w + Arg(z / w)"
  using assms
  by (auto simp: Arg_ge_0 intro: Arg_le_div_sum)

lemma Arg_diff:
  assumes "w \<noteq> 0" "z \<noteq> 0"
    shows "Arg w - Arg z = (if Arg z \<le> Arg w then Arg(w / z) else Arg(w/z) - 2*pi)"
  using assms
  apply (auto simp: Arg_ge_0 Arg_divide not_le)
  using Arg_divide [of w z] Arg_inverse [of "w/z"]
  apply auto
  by (metis Arg_eq_0 less_irrefl minus_diff_eq right_minus_eq)

lemma Arg_add:
  assumes "w \<noteq> 0" "z \<noteq> 0"
    shows "Arg w + Arg z = (if Arg w + Arg z < 2*pi then Arg(w * z) else Arg(w * z) + 2*pi)"
  using assms
  using Arg_diff [of "w*z" z] Arg_le_div_sum_eq [of z "w*z"]
  apply (auto simp: Arg_ge_0 Arg_divide not_le)
  apply (metis Arg_lt_2pi add.commute)
  apply (metis (no_types) Arg add.commute diff_0 diff_add_cancel diff_less_eq diff_minus_eq_add not_less)
  done

lemma Arg_times:
  assumes "w \<noteq> 0" "z \<noteq> 0"
    shows "Arg (w * z) = (if Arg w + Arg z < 2*pi then Arg w + Arg z
                            else (Arg w + Arg z) - 2*pi)"
  using Arg_add [OF assms]
  by auto

lemma Arg_cnj: "Arg(cnj z) = (if z \<in> \<real> \<and> 0 \<le> Re z then Arg z else 2*pi - Arg z)"
  apply (cases "z=0", simp)
  apply (rule trans [of _ "Arg(inverse z)"])
  apply (simp add: Arg_eq_iff divide_simps complex_norm_square [symmetric] mult.commute)
  apply (metis norm_eq_zero of_real_power zero_less_power2)
  apply (auto simp: of_real_numeral Arg_inverse)
  done

lemma Arg_real: "z \<in> \<real> \<Longrightarrow> Arg z = (if 0 \<le> Re z then 0 else pi)"
  using Arg_eq_0 Arg_eq_0_pi
  by auto

lemma Arg_exp: "0 \<le> Im z \<Longrightarrow> Im z < 2*pi \<Longrightarrow> Arg(exp z) = Im z"
  by (rule Arg_unique [of  "exp(Re z)"]) (auto simp: Exp_eq_polar)


subsection{*Analytic properties of tangent function*}

lemma cnj_tan: "cnj(tan z) = tan(cnj z)"
  by (simp add: cnj_cos cnj_sin tan_def)

lemma complex_differentiable_at_tan: "~(cos z = 0) \<Longrightarrow> tan complex_differentiable at z"
  unfolding complex_differentiable_def
  using DERIV_tan by blast

lemma complex_differentiable_within_tan: "~(cos z = 0)
         \<Longrightarrow> tan complex_differentiable (at z within s)"
  using complex_differentiable_at_tan complex_differentiable_at_within by blast

lemma continuous_within_tan: "~(cos z = 0) \<Longrightarrow> continuous (at z within s) tan"
  using continuous_at_imp_continuous_within isCont_tan by blast

lemma continuous_on_tan [continuous_intros]: "(\<And>z. z \<in> s \<Longrightarrow> ~(cos z = 0)) \<Longrightarrow> continuous_on s tan"
  by (simp add: continuous_at_imp_continuous_on)

lemma holomorphic_on_tan: "(\<And>z. z \<in> s \<Longrightarrow> ~(cos z = 0)) \<Longrightarrow> tan holomorphic_on s"
  by (simp add: complex_differentiable_within_tan holomorphic_on_def)


subsection{*Complex logarithms (the conventional principal value)*}

definition Ln where
   "Ln \<equiv> \<lambda>z. THE w. exp w = z & -pi < Im(w) & Im(w) \<le> pi"

lemma
  assumes "z \<noteq> 0"
    shows exp_Ln [simp]: "exp(Ln z) = z"
      and mpi_less_Im_Ln: "-pi < Im(Ln z)"
      and Im_Ln_le_pi:    "Im(Ln z) \<le> pi"
proof -
  obtain \<psi> where z: "z / (cmod z) = Complex (cos \<psi>) (sin \<psi>)"
    using complex_unimodular_polar [of "z / (norm z)"] assms
    by (auto simp: norm_divide divide_simps)
  obtain \<phi> where \<phi>: "- pi < \<phi>" "\<phi> \<le> pi" "sin \<phi> = sin \<psi>" "cos \<phi> = cos \<psi>"
    using sincos_principal_value [of "\<psi>"] assms
    by (auto simp: norm_divide divide_simps)
  have "exp(Ln z) = z & -pi < Im(Ln z) & Im(Ln z) \<le> pi" unfolding Ln_def
    apply (rule theI [where a = "Complex (ln(norm z)) \<phi>"])
    using z assms \<phi>
    apply (auto simp: field_simps exp_complex_eqI Exp_eq_polar cis.code)
    done
  then show "exp(Ln z) = z" "-pi < Im(Ln z)" "Im(Ln z) \<le> pi"
    by auto
qed

lemma Ln_exp [simp]:
  assumes "-pi < Im(z)" "Im(z) \<le> pi"
    shows "Ln(exp z) = z"
  apply (rule exp_complex_eqI)
  using assms mpi_less_Im_Ln  [of "exp z"] Im_Ln_le_pi [of "exp z"]
  apply auto
  done

lemma Ln_eq_iff: "w \<noteq> 0 \<Longrightarrow> z \<noteq> 0 \<Longrightarrow> (Ln w = Ln z \<longleftrightarrow> w = z)"
  by (metis exp_Ln)

lemma Ln_unique: "exp(z) = w \<Longrightarrow> -pi < Im(z) \<Longrightarrow> Im(z) \<le> pi \<Longrightarrow> Ln w = z"
  using Ln_exp by blast

lemma Re_Ln [simp]: "z \<noteq> 0 \<Longrightarrow> Re(Ln z) = ln(norm z)"
by (metis exp_Ln assms ln_exp norm_exp_eq_Re)

lemma exists_complex_root:
  fixes a :: complex
  shows "n \<noteq> 0 \<Longrightarrow> \<exists>z. z ^ n = a"
  apply (cases "a=0", simp)
  apply (rule_tac x= "exp(Ln(a) / n)" in exI)
  apply (auto simp: exp_of_nat_mult [symmetric])
  done

subsection{*The Unwinding Number and the Ln-product Formula*}

text{*Note that in this special case the unwinding number is -1, 0 or 1.*}

definition unwinding :: "complex \<Rightarrow> complex" where
   "unwinding(z) = (z - Ln(exp z)) / (of_real(2*pi) * ii)"

lemma unwinding_2pi: "(2*pi) * ii * unwinding(z) = z - Ln(exp z)"
  by (simp add: unwinding_def)

lemma Ln_times_unwinding:
    "w \<noteq> 0 \<Longrightarrow> z \<noteq> 0 \<Longrightarrow> Ln(w * z) = Ln(w) + Ln(z) - (2*pi) * ii * unwinding(Ln w + Ln z)"
  using unwinding_2pi by (simp add: exp_add)


subsection{*Derivative of Ln away from the branch cut*}

lemma
  assumes "Im(z) = 0 \<Longrightarrow> 0 < Re(z)"
    shows has_field_derivative_Ln: "(Ln has_field_derivative inverse(z)) (at z)"
      and Im_Ln_less_pi:           "Im (Ln z) < pi"
proof -
  have znz: "z \<noteq> 0"
    using assms by auto
  then show *: "Im (Ln z) < pi" using assms
    by (metis exp_Ln Im_Ln_le_pi Im_exp Re_exp abs_of_nonneg cmod_eq_Re cos_pi mult.right_neutral mult_minus_right mult_zero_right neg_less_0_iff_less norm_exp_eq_Re not_less not_less_iff_gr_or_eq sin_pi)
  show "(Ln has_field_derivative inverse(z)) (at z)"
    apply (rule has_complex_derivative_inverse_strong_x
              [where f = exp and s = "{w. -pi < Im(w) & Im(w) < pi}"])
    using znz *
    apply (auto simp: continuous_on_exp open_Collect_conj open_halfspace_Im_gt open_halfspace_Im_lt)
    apply (metis DERIV_exp exp_Ln)
    apply (metis mpi_less_Im_Ln)
    done
qed

declare has_field_derivative_Ln [derivative_intros]
declare has_field_derivative_Ln [THEN DERIV_chain2, derivative_intros]

lemma complex_differentiable_at_Ln: "(Im(z) = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> Ln complex_differentiable at z"
  using complex_differentiable_def has_field_derivative_Ln by blast

lemma complex_differentiable_within_Ln: "(Im(z) = 0 \<Longrightarrow> 0 < Re(z))
         \<Longrightarrow> Ln complex_differentiable (at z within s)"
  using complex_differentiable_at_Ln complex_differentiable_within_subset by blast

lemma continuous_at_Ln: "(Im(z) = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous (at z) Ln"
  by (simp add: complex_differentiable_imp_continuous_at complex_differentiable_within_Ln)

lemma isCont_Ln' [simp]:
   "\<lbrakk>isCont f z; Im(f z) = 0 \<Longrightarrow> 0 < Re(f z)\<rbrakk> \<Longrightarrow> isCont (\<lambda>x. Ln (f x)) z"
  by (blast intro: isCont_o2 [OF _ continuous_at_Ln])

lemma continuous_within_Ln: "(Im(z) = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous (at z within s) Ln"
  using continuous_at_Ln continuous_at_imp_continuous_within by blast

lemma continuous_on_Ln [continuous_intros]: "(\<And>z. z \<in> s \<Longrightarrow> Im(z) = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous_on s Ln"
  by (simp add: continuous_at_imp_continuous_on continuous_within_Ln)

lemma holomorphic_on_Ln: "(\<And>z. z \<in> s \<Longrightarrow> Im(z) = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> Ln holomorphic_on s"
  by (simp add: complex_differentiable_within_Ln holomorphic_on_def)


subsection{*Relation to Real Logarithm*}

lemma Ln_of_real:
  assumes "0 < z"
    shows "Ln(of_real z) = of_real(ln z)"
proof -
  have "Ln(of_real (exp (ln z))) = Ln (exp (of_real (ln z)))"
    by (simp add: exp_of_real)
  also have "... = of_real(ln z)"
    using assms
    by (subst Ln_exp) auto
  finally show ?thesis
    using assms by simp
qed

corollary Ln_in_Reals [simp]: "z \<in> \<real> \<Longrightarrow> Re z > 0 \<Longrightarrow> Ln z \<in> \<real>"
  by (auto simp: Ln_of_real elim: Reals_cases)


subsection{*Quadrant-type results for Ln*}

lemma cos_lt_zero_pi: "pi/2 < x \<Longrightarrow> x < 3*pi/2 \<Longrightarrow> cos x < 0"
  using cos_minus_pi cos_gt_zero_pi [of "x-pi"]
  by simp

lemma Re_Ln_pos_lt:
  assumes "z \<noteq> 0"
    shows "abs(Im(Ln z)) < pi/2 \<longleftrightarrow> 0 < Re(z)"
proof -
  { fix w
    assume "w = Ln z"
    then have w: "Im w \<le> pi" "- pi < Im w"
      using Im_Ln_le_pi [of z]  mpi_less_Im_Ln [of z]  assms
      by auto
    then have "abs(Im w) < pi/2 \<longleftrightarrow> 0 < Re(exp w)"
      apply (auto simp: Re_exp zero_less_mult_iff cos_gt_zero_pi)
      using cos_lt_zero_pi [of "-(Im w)"] cos_lt_zero_pi [of "(Im w)"]
      apply (simp add: abs_if split: split_if_asm)
      apply (metis (no_types) cos_minus cos_pi_half eq_divide_eq_numeral1(1) eq_numeral_simps(4)
               less_numeral_extra(3) linorder_neqE_linordered_idom minus_mult_minus minus_mult_right
               mult_numeral_1_right)
      done
  }
  then show ?thesis using assms
    by auto
qed

lemma Re_Ln_pos_le:
  assumes "z \<noteq> 0"
    shows "abs(Im(Ln z)) \<le> pi/2 \<longleftrightarrow> 0 \<le> Re(z)"
proof -
  { fix w
    assume "w = Ln z"
    then have w: "Im w \<le> pi" "- pi < Im w"
      using Im_Ln_le_pi [of z]  mpi_less_Im_Ln [of z]  assms
      by auto
    then have "abs(Im w) \<le> pi/2 \<longleftrightarrow> 0 \<le> Re(exp w)"
      apply (auto simp: Re_exp zero_le_mult_iff cos_ge_zero)
      using cos_lt_zero_pi [of "- (Im w)"] cos_lt_zero_pi [of "(Im w)"] not_le
      apply (auto simp: abs_if split: split_if_asm)
      done
  }
  then show ?thesis using assms
    by auto
qed

lemma Im_Ln_pos_lt:
  assumes "z \<noteq> 0"
    shows "0 < Im(Ln z) \<and> Im(Ln z) < pi \<longleftrightarrow> 0 < Im(z)"
proof -
  { fix w
    assume "w = Ln z"
    then have w: "Im w \<le> pi" "- pi < Im w"
      using Im_Ln_le_pi [of z]  mpi_less_Im_Ln [of z]  assms
      by auto
    then have "0 < Im w \<and> Im w < pi \<longleftrightarrow> 0 < Im(exp w)"
      using sin_gt_zero [of "- (Im w)"] sin_gt_zero [of "(Im w)"]
      apply (auto simp: Im_exp zero_less_mult_iff)
      using less_linear apply fastforce
      using less_linear apply fastforce
      done
  }
  then show ?thesis using assms
    by auto
qed

lemma Im_Ln_pos_le:
  assumes "z \<noteq> 0"
    shows "0 \<le> Im(Ln z) \<and> Im(Ln z) \<le> pi \<longleftrightarrow> 0 \<le> Im(z)"
proof -
  { fix w
    assume "w = Ln z"
    then have w: "Im w \<le> pi" "- pi < Im w"
      using Im_Ln_le_pi [of z]  mpi_less_Im_Ln [of z]  assms
      by auto
    then have "0 \<le> Im w \<and> Im w \<le> pi \<longleftrightarrow> 0 \<le> Im(exp w)"
      using sin_ge_zero [of "- (Im w)"] sin_ge_zero [of "(Im w)"]
      apply (auto simp: Im_exp zero_le_mult_iff sin_ge_zero)
      apply (metis not_le not_less_iff_gr_or_eq pi_not_less_zero sin_eq_0_pi)
      done }
  then show ?thesis using assms
    by auto
qed

lemma Re_Ln_pos_lt_imp: "0 < Re(z) \<Longrightarrow> abs(Im(Ln z)) < pi/2"
  by (metis Re_Ln_pos_lt less_irrefl zero_complex.simps(1))

lemma Im_Ln_pos_lt_imp: "0 < Im(z) \<Longrightarrow> 0 < Im(Ln z) \<and> Im(Ln z) < pi"
  by (metis Im_Ln_pos_lt not_le order_refl zero_complex.simps(2))

lemma Im_Ln_eq_0: "z \<noteq> 0 \<Longrightarrow> (Im(Ln z) = 0 \<longleftrightarrow> 0 < Re(z) \<and> Im(z) = 0)"
  by (metis exp_Ln Im_Ln_less_pi Im_Ln_pos_le Im_Ln_pos_lt Re_complex_of_real add.commute add.left_neutral
       complex_eq exp_of_real le_less mult_zero_right norm_exp_eq_Re norm_le_zero_iff not_le of_real_0)

lemma Im_Ln_eq_pi: "z \<noteq> 0 \<Longrightarrow> (Im(Ln z) = pi \<longleftrightarrow> Re(z) < 0 \<and> Im(z) = 0)"
  by (metis Im_Ln_eq_0 Im_Ln_less_pi Im_Ln_pos_le Im_Ln_pos_lt add.right_neutral complex_eq mult_zero_right not_less not_less_iff_gr_or_eq of_real_0)


subsection{*More Properties of Ln*}

lemma cnj_Ln: "(Im z = 0 \<Longrightarrow> 0 < Re z) \<Longrightarrow> cnj(Ln z) = Ln(cnj z)"
  apply (cases "z=0", auto)
  apply (rule exp_complex_eqI)
  apply (auto simp: abs_if split: split_if_asm)
  apply (metis Im_Ln_less_pi add_mono_thms_linordered_field(5) cnj.simps(1) cnj.simps(2) mult_2 neg_equal_0_iff_equal)
  apply (metis add_mono_thms_linordered_field(5) complex_cnj_zero_iff diff_0_right diff_minus_eq_add minus_diff_eq mpi_less_Im_Ln mult.commute mult_2_right neg_less_iff_less)
  by (metis exp_Ln exp_cnj)

lemma Ln_inverse: "(Im(z) = 0 \<Longrightarrow> 0 < Re z) \<Longrightarrow> Ln(inverse z) = -(Ln z)"
  apply (cases "z=0", auto)
  apply (rule exp_complex_eqI)
  using mpi_less_Im_Ln [of z] mpi_less_Im_Ln [of "inverse z"]
  apply (auto simp: abs_if exp_minus split: split_if_asm)
  apply (metis Im_Ln_less_pi Im_Ln_pos_le add_less_cancel_left add_strict_mono
               inverse_inverse_eq inverse_zero le_less mult.commute mult_2_right)
  done

lemma Ln_1 [simp]: "Ln(1) = 0"
proof -
  have "Ln (exp 0) = 0"
    by (metis exp_zero ln_exp Ln_of_real of_real_0 of_real_1 zero_less_one)
  then show ?thesis
    by simp
qed

lemma Ln_minus1 [simp]: "Ln(-1) = ii * pi"
  apply (rule exp_complex_eqI)
  using Im_Ln_le_pi [of "-1"] mpi_less_Im_Ln [of "-1"] cis_conv_exp cis_pi
  apply (auto simp: abs_if)
  done

lemma Ln_ii [simp]: "Ln ii = ii * of_real pi/2"
  using Ln_exp [of "ii * (of_real pi/2)"]
  unfolding exp_Euler
  by simp

lemma Ln_minus_ii [simp]: "Ln(-ii) = - (ii * pi/2)"
proof -
  have  "Ln(-ii) = Ln(1/ii)"
    by simp
  also have "... = - (Ln ii)"
    by (metis Ln_inverse ii.sel(2) inverse_eq_divide zero_neq_one)
  also have "... = - (ii * pi/2)"
    by (simp add: Ln_ii)
  finally show ?thesis .
qed

lemma Ln_times:
  assumes "w \<noteq> 0" "z \<noteq> 0"
    shows "Ln(w * z) =
                (if Im(Ln w + Ln z) \<le> -pi then
                  (Ln(w) + Ln(z)) + ii * of_real(2*pi)
                else if Im(Ln w + Ln z) > pi then
                  (Ln(w) + Ln(z)) - ii * of_real(2*pi)
                else Ln(w) + Ln(z))"
  using pi_ge_zero Im_Ln_le_pi [of w] Im_Ln_le_pi [of z]
  using assms mpi_less_Im_Ln [of w] mpi_less_Im_Ln [of z]
  by (auto simp: of_real_numeral exp_add exp_diff sin_double cos_double exp_Euler intro!: Ln_unique)

lemma Ln_times_simple:
    "\<lbrakk>w \<noteq> 0; z \<noteq> 0; -pi < Im(Ln w) + Im(Ln z); Im(Ln w) + Im(Ln z) \<le> pi\<rbrakk>
         \<Longrightarrow> Ln(w * z) = Ln(w) + Ln(z)"
  by (simp add: Ln_times)

lemma Ln_minus:
  assumes "z \<noteq> 0"
    shows "Ln(-z) = (if Im(z) \<le> 0 \<and> ~(Re(z) < 0 \<and> Im(z) = 0)
                     then Ln(z) + ii * pi
                     else Ln(z) - ii * pi)" (is "_ = ?rhs")
  using Im_Ln_le_pi [of z] mpi_less_Im_Ln [of z] assms
        Im_Ln_eq_pi [of z] Im_Ln_pos_lt [of z]
    by (auto simp: of_real_numeral exp_add exp_diff exp_Euler intro!: Ln_unique)

lemma Ln_inverse_if:
  assumes "z \<noteq> 0"
    shows "Ln (inverse z) =
            (if (Im(z) = 0 \<longrightarrow> 0 < Re z)
             then -(Ln z)
             else -(Ln z) + \<i> * 2 * complex_of_real pi)"
proof (cases "(Im(z) = 0 \<longrightarrow> 0 < Re z)")
  case True then show ?thesis
    by (simp add: Ln_inverse)
next
  case False
  then have z: "Im z = 0" "Re z < 0"
    using assms
    apply auto
    by (metis cnj.code complex_cnj_cnj not_less_iff_gr_or_eq zero_complex.simps(1) zero_complex.simps(2))
  have "Ln(inverse z) = Ln(- (inverse (-z)))"
    by simp
  also have "... = Ln (inverse (-z)) + \<i> * complex_of_real pi"
    using assms z
    apply (simp add: Ln_minus)
    apply (simp add: field_simps)
    done
  also have "... = - Ln (- z) + \<i> * complex_of_real pi"
    apply (subst Ln_inverse)
    using z assms by auto
  also have "... = - (Ln z) + \<i> * 2 * complex_of_real pi"
    apply (subst Ln_minus [OF assms])
    using assms z
    apply simp
    done
  finally show ?thesis
    using assms z
    by simp
qed

lemma Ln_times_ii:
  assumes "z \<noteq> 0"
    shows  "Ln(ii * z) = (if 0 \<le> Re(z) | Im(z) < 0
                          then Ln(z) + ii * of_real pi/2
                          else Ln(z) - ii * of_real(3 * pi/2))"
  using Im_Ln_le_pi [of z] mpi_less_Im_Ln [of z] assms
        Im_Ln_eq_pi [of z] Im_Ln_pos_lt [of z] Re_Ln_pos_le [of z]
  by (auto simp: of_real_numeral Ln_times)


subsection{*Relation between Square Root and exp/ln, hence its derivative*}

lemma csqrt_exp_Ln:
  assumes "z \<noteq> 0"
    shows "csqrt z = exp(Ln(z) / 2)"
proof -
  have "(exp (Ln z / 2))\<^sup>2 = (exp (Ln z))"
    by (metis exp_double nonzero_mult_divide_cancel_left times_divide_eq_right zero_neq_numeral)
  also have "... = z"
    using assms exp_Ln by blast
  finally have "csqrt z = csqrt ((exp (Ln z / 2))\<^sup>2)"
    by simp
  also have "... = exp (Ln z / 2)"
    apply (subst csqrt_square)
    using cos_gt_zero_pi [of "(Im (Ln z) / 2)"] Im_Ln_le_pi mpi_less_Im_Ln assms
    apply (auto simp: Re_exp Im_exp zero_less_mult_iff zero_le_mult_iff, fastforce+)
    done
  finally show ?thesis using assms csqrt_square
    by simp
qed

lemma csqrt_inverse:
  assumes "Im(z) = 0 \<Longrightarrow> 0 < Re z"
    shows "csqrt (inverse z) = inverse (csqrt z)"
proof (cases "z=0", simp)
  assume "z \<noteq> 0 "
  then show ?thesis
    using assms
    by (simp add: csqrt_exp_Ln Ln_inverse exp_minus)
qed

lemma cnj_csqrt:
  assumes "Im z = 0 \<Longrightarrow> 0 \<le> Re(z)"
    shows "cnj(csqrt z) = csqrt(cnj z)"
proof (cases "z=0", simp)
  assume z: "z \<noteq> 0"
  then have "Im z = 0 \<Longrightarrow> 0 < Re(z)"
    using assms cnj.code complex_cnj_zero_iff by fastforce
  then show ?thesis
   using z by (simp add: csqrt_exp_Ln cnj_Ln exp_cnj)
qed

lemma has_field_derivative_csqrt:
  assumes "Im z = 0 \<Longrightarrow> 0 < Re(z)"
    shows "(csqrt has_field_derivative inverse(2 * csqrt z)) (at z)"
proof -
  have z: "z \<noteq> 0"
    using assms by auto
  then have *: "inverse z = inverse (2*z) * 2"
    by (simp add: divide_simps)
  show ?thesis
    apply (rule DERIV_transform_at [where f = "\<lambda>z. exp(Ln(z) / 2)" and d = "norm z"])
    apply (intro derivative_eq_intros | simp add: assms)+
    apply (rule *)
    using z
    apply (auto simp: field_simps csqrt_exp_Ln [symmetric])
    apply (metis power2_csqrt power2_eq_square)
    apply (metis csqrt_exp_Ln dist_0_norm less_irrefl)
    done
qed

lemma complex_differentiable_at_csqrt:
    "(Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> csqrt complex_differentiable at z"
  using complex_differentiable_def has_field_derivative_csqrt by blast

lemma complex_differentiable_within_csqrt:
    "(Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> csqrt complex_differentiable (at z within s)"
  using complex_differentiable_at_csqrt complex_differentiable_within_subset by blast

lemma continuous_at_csqrt:
    "(Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous (at z) csqrt"
  by (simp add: complex_differentiable_within_csqrt complex_differentiable_imp_continuous_at)

corollary isCont_csqrt' [simp]:
   "\<lbrakk>isCont f z; Im(f z) = 0 \<Longrightarrow> 0 < Re(f z)\<rbrakk> \<Longrightarrow> isCont (\<lambda>x. csqrt (f x)) z"
  by (blast intro: isCont_o2 [OF _ continuous_at_csqrt])

lemma continuous_within_csqrt:
    "(Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous (at z within s) csqrt"
  by (simp add: complex_differentiable_imp_continuous_at complex_differentiable_within_csqrt)

lemma continuous_on_csqrt [continuous_intros]:
    "(\<And>z. z \<in> s \<and> Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> continuous_on s csqrt"
  by (simp add: continuous_at_imp_continuous_on continuous_within_csqrt)

lemma holomorphic_on_csqrt:
    "(\<And>z. z \<in> s \<and> Im z = 0 \<Longrightarrow> 0 < Re(z)) \<Longrightarrow> csqrt holomorphic_on s"
  by (simp add: complex_differentiable_within_csqrt holomorphic_on_def)

lemma continuous_within_closed_nontrivial:
    "closed s \<Longrightarrow> a \<notin> s ==> continuous (at a within s) f"
  using open_Compl
  by (force simp add: continuous_def eventually_at_topological filterlim_iff open_Collect_neg)

lemma closed_Real_halfspace_Re_ge: "closed (\<real> \<inter> {w. x \<le> Re(w)})"
  using closed_halfspace_Re_ge
  by (simp add: closed_Int closed_complex_Reals)

lemma continuous_within_csqrt_posreal:
    "continuous (at z within (\<real> \<inter> {w. 0 \<le> Re(w)})) csqrt"
proof (cases "Im z = 0 --> 0 < Re(z)")
  case True then show ?thesis
    by (blast intro: continuous_within_csqrt)
next
  case False
  then have "Im z = 0" "Re z < 0 \<or> z = 0"
    using False cnj.code complex_cnj_zero_iff by auto force
  then show ?thesis
    apply (auto simp: continuous_within_closed_nontrivial [OF closed_Real_halfspace_Re_ge])
    apply (auto simp: continuous_within_eps_delta norm_conv_dist [symmetric])
    apply (rule_tac x="e^2" in exI)
    apply (auto simp: Reals_def)
by (metis linear not_less real_sqrt_less_iff real_sqrt_pow2_iff real_sqrt_power)
qed


end
