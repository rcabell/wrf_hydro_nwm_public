import datetime
import pathlib
import pickle
import time

import pytest

from tests.utilities import DEFAULT_LOOP_WAIT, ONE_HOUR

candidate_run_file = pathlib.Path('run_candidate/WrfHydroSim_collected.pkl')


# regression question
def test_output_has_nans(output_dir, xrcmp_n_cores):
    print("\nQuestion: Outputs from all tests are free of nans in data and attributes\n", end="")
    print('\n')

    # TODO: there's probably a better way to do this
    output_files_expected = 4  # TODO: verify this?
    sim_files = []
    start_wait = datetime.datetime.now()
    while len(sim_files) < output_files_expected:
        time.sleep(DEFAULT_LOOP_WAIT)
        if (datetime.datetime.now() - start_wait).seconds > ONE_HOUR:
            pytest.skip("test_output_has_nans timed out waiting for runs to complete")

        sim_files = list(output_dir.rglob('WrfHydroSim_collected.pkl'))

    for file in sim_files:
        sim = pickle.load(file.open('rb'))
        sim_nans = sim.output.check_output_nans(n_cores=xrcmp_n_cores)
        assert sim_nans is None, \
            'nans found in the following files ' + sim_nans['file'].unique()
