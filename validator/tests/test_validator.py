"""Tests for Validator"""
import pytest
from delphi_validator.errors import ValidationFailure
from delphi_validator.validate import Validator

class TestValidatorInitialization:
    """Tests for proper initialization."""
    def test_default_settings(self):
        """Test default initialization."""
        params = {
            "global": {
                "export_dir": None
            },
            "validation": {
                "global": {
                    "data_source": "",
                    "span_length": 0,
                    "end_date": "2020-09-01"
                }
            }
        }
        validator = Validator(params)
        assert len(validator.suppressed_errors) == 0
        assert isinstance(validator.suppressed_errors, list)

    def test_suppressed_errors(self):
        """Test initialization with suppressed errors."""
        params = {
            "global": {
                "export_dir": None
            },
            "validation": {
                "global": {
                    "data_source": "",
                    "span_length": 0,
                    "end_date": "2020-09-01",
                    "suppressed_errors": [{"check_name": "a",
                                        "date": None,
                                        "signal": "b"},
                                        {"check_name":"c",
                                        "date": None,
                                        "geo_type": "d"}]
                }
            }
        }

        validator = Validator(params)
        assert validator.suppressed_errors == [ValidationFailure("a", None, None, "b"),
                                               ValidationFailure("c", None, "d", "None")]

    def test_incorrect_suppressed_errors(self):
        """Test initialization with improperly coded suppressed errors."""
        with pytest.raises(AssertionError, match='suppressed_errors may only have fields '
                                                 '"check_name", "date", "geo_type", "signal"'):
            # entry with invalid keys
            Validator({
                "global": {
                    "export_dir": None
                },
                "validation": {
                    "global": {
                        "data_source": "",
                        "span_length": 0,
                        "end_date": "2020-09-01",
                        "suppressed_errors": [{"check_name": "a",
                                            "date": None,
                                            "signal": "b"},
                                            {"check_name":"c",
                                            "date": None,
                                            "geo_type": "d"},
                                            {"check_name": "a",
                                            "fake": "b"}]
                    }
                }
            })

        with pytest.raises(AssertionError, match="suppressed_errors must be a list of objects"):
            # entry that is not a list
            Validator({
                "global": {
                    "export_dir": None
                },
                "validation": {
                    "global": {
                    "data_source": "",
                    "span_length": 0,
                    "end_date": "2020-09-01",
                    "suppressed_errors": [{"check_name": "a",
                                           "date": None,
                                           "signal": "b"},
                                          {"check_name":"c",
                                           "date": None,
                                           "geo_type": "d"},
                                          ["ab"]]
                    }
                }
            })
