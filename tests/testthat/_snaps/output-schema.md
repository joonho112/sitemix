# public output schema snapshots cover supported families and variants

    Code
      snapshot_schema("A: binomial counts", a)
    Output
      ## A: binomial counts
      Attributes
       rows columns object_class                            family   aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       2    18      sitemix_estimates/tbl_df/tbl/data.frame binomial NULL           NULL              NULL                NULL      summary_uncertainty FALSE FALSE TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
      Lexicon
       column                    values
       indicator                 absent
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst
       input_mode                counts_full_suff
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability FALSE, TRUE
    Code
      snapshot_schema("B: multivariate counts with V/K", b)
    Output
      ## B: multivariate counts with V/K
      Attributes
       rows columns object_class                            family       aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       2    20      sitemix_estimates/tbl_df/tbl/data.frame multivariate NULL           NULL              NULL                NULL      summary_uncertainty TRUE  TRUE  TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       V                         list      list
       K                         integer   integer
      Lexicon
       column                    values
       indicator                 frpm, snap
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst
       input_mode                counts_full_suff
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability TRUE
      V metadata
       row V_class family       vcov_method vcov_scale matrix_dim matrix_rank indicator_order
       1   sm_vcov multivariate sur         raw        2x2        2           snap|frpm
       2   sm_vcov multivariate sur         raw        2x2        2           snap|frpm
    Code
      snapshot_schema("C: multinomial counts with V/K", c)
    Output
      ## C: multinomial counts with V/K
      Attributes
       rows columns object_class                            family      aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       3    20      sitemix_estimates/tbl_df/tbl/data.frame multinomial NULL           NULL              NULL                NULL      summary_uncertainty TRUE  TRUE  TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       V                         list      list
       K                         integer   integer
      Lexicon
       column                    values
       indicator                 eng, oth, spa
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst
       input_mode                counts_full_suff
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability TRUE
      V metadata
       row V_class family      vcov_method vcov_scale matrix_dim matrix_rank indicator_order
       1   sm_vcov multinomial multinomial raw        3x3        2           eng|spa|oth
       2   sm_vcov multinomial multinomial raw        3x3        2           eng|spa|oth
       3   sm_vcov multinomial multinomial raw        3x3        2           eng|spa|oth
    Code
      snapshot_schema("D0: aggregate binomial with V", d0)
    Output
      ## D0: aggregate binomial with V
      Attributes
       rows columns object_class                            family   aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       2    19      sitemix_estimates/tbl_df/tbl/data.frame binomial D0             NULL              NULL                NULL      summary_uncertainty TRUE  FALSE TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       V                         list      list
      Lexicon
       column                    values
       indicator                 absent
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst
       input_mode                aggregate
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability FALSE, TRUE
      V metadata
       row V_class family   vcov_method vcov_scale    matrix_dim matrix_rank indicator_order
       1   sm_vcov binomial <NA>        arcsine_delta 1x1        1           absent
       2   sm_vcov binomial <NA>        arcsine_delta 1x1        1           absent
    Code
      snapshot_schema("D1: aggregate marginals with working-independence V/K", d1)
    Output
      ## D1: aggregate marginals with working-independence V/K
      Attributes
       rows columns object_class                            family       aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       4    20      sitemix_estimates/tbl_df/tbl/data.frame multivariate D1             unknown           common              unknown   summary_uncertainty TRUE  TRUE  TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       V                         list      list
       K                         integer   integer
      Lexicon
       column                    values
       indicator                 frpm, snap
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst
       input_mode                aggregate
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability FALSE
      V metadata
       row V_class family       vcov_method          vcov_scale    matrix_dim matrix_rank indicator_order
       1   sm_vcov multivariate working_independence arcsine_delta 2x2        2           frpm|snap
       2   sm_vcov multivariate working_independence arcsine_delta 2x2        2           frpm|snap
       3   sm_vcov multivariate working_independence arcsine_delta 2x2        2           frpm|snap
       4   sm_vcov multivariate working_independence arcsine_delta 2x2        2           frpm|snap
    Code
      snapshot_schema("smoothing: overwrite audit trail", smoothed)
    Output
      ## smoothing: overwrite audit trail
      Attributes
       rows columns object_class                            family   aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       16   21      sitemix_estimates/tbl_df/tbl/data.frame binomial NULL           NULL              NULL                NULL      summary_uncertainty FALSE FALSE TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       se_smoothed               double    numeric
       var_method_smoothed       character character
       se_pre_smoothing          double    numeric
      Lexicon
       column                    values
       indicator                 absent
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst + gvf_smooth_loglinear
       input_mode                counts_full_suff
       framing                   NA
       flag_small_n              FALSE
       flag_zero_cell            FALSE
       flag_suppressed           FALSE
       flag_below_accountability TRUE
    Code
      snapshot_schema("suppression: drop", suppressed_drop)
    Output
      ## suppression: drop
      Attributes
       rows columns object_class                            family   aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       3    25      sitemix_estimates/tbl_df/tbl/data.frame binomial D0             NULL              NULL                NULL      summary_uncertainty FALSE FALSE TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       estimate_status           character character
       sensitivity_probability   double    numeric
       sensitivity_var_raw       double    numeric
       sensitivity_var           double    numeric
       sensitivity_n             integer   integer
       sensitivity_method        character character
       sensitivity_acknowledged  logical   logical
      Lexicon
       column                    values
       indicator                 absent
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst, suppressed_drop
       input_mode                aggregate
       framing                   NA
       flag_small_n              FALSE, TRUE
       flag_zero_cell            FALSE, NA, TRUE
       flag_suppressed           FALSE, TRUE
       flag_below_accountability FALSE, TRUE
    Code
      snapshot_schema("suppression: acknowledged variance sensitivity",
        suppressed_upper)
    Output
      ## suppression: acknowledged variance sensitivity
      Attributes
       rows columns object_class                            family   aggregate_case sampling_relation denominator_pattern d1_regime sitemix_role        has_V has_K valid
       3    25      sitemix_estimates/tbl_df/tbl/data.frame binomial D0             NULL              NULL                NULL      summary_uncertainty FALSE FALSE TRUE
      Columns
       column                    typeof    class
       site_id                   character character
       year                      integer   integer
       indicator                 character character
       theta_raw                 double    numeric
       theta_hat                 double    numeric
       se_raw                    double    numeric
       se                        double    numeric
       n                         integer   integer
       n_eff                     double    numeric
       estimate_scale            character character
       transform                 character character
       var_method                character character
       flag_small_n              logical   logical
       flag_zero_cell            logical   logical
       input_mode                character character
       flag_suppressed           logical   logical
       framing                   character character
       flag_below_accountability logical   logical
       estimate_status           character character
       sensitivity_probability   double    numeric
       sensitivity_var_raw       double    numeric
       sensitivity_var           double    numeric
       sensitivity_n             integer   integer
       sensitivity_method        character character
       sensitivity_acknowledged  logical   logical
      Lexicon
       column                    values
       indicator                 absent
       estimate_scale            arcsine
       transform                 arcsine
       var_method                arcsine_vst, suppression_sensitivity
       input_mode                aggregate
       framing                   NA
       flag_small_n              FALSE, TRUE
       flag_zero_cell            FALSE, NA, TRUE
       flag_suppressed           FALSE, TRUE
       flag_below_accountability FALSE, TRUE

