import pytest

from os.path import join

import pandas as pd
from delphi_cds_testing.pull import pull_cds_data

pop_df = pd.read_csv(
    join("..", "static", "fips_population.csv"),
    dtype={"fips": int, "population": float}
)

countyname_to_fips_df = pd.read_csv(
        join("..", "static", "countyname_to_fips.csv"), dtype={"fips": int}
    )[["fips", "name"]]



class TestPullCDS:
    def test_good_file(self):

        df = pull_cds_data(join("test_data", "small.csv"), countyname_to_fips_df, pop_df)

        assert (
            df.columns.values
            == ["fips", "timestamp", "population", "new_counts", "cumulative_counts"]
        ).all()

    def test_missing_cols(self):

        with pytest.raises(ValueError):
            df = pull_cds_data(
                join("test_data", "bad_missing_cols.csv"), countyname_to_fips_df, pop_df
            )

    def test_extra_cols(self):

        with pytest.raises(ValueError):
            df = pull_cds_data(
                join("test_data", "bad_extra_cols.csv"), countyname_to_fips_df, pop_df
            )