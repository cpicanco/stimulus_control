# -*- coding: utf-8 -*-
"""
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>. 
"""

# for linux only

from subprocess import check_output,CalledProcessError,STDOUT
import os
import shutil
from glob import glob

def get_tag_commit():
    """
    origial script from here:
    https://github.com/pupil-labs/pupil/blob/master/pupil_src/shared_modules/version_utils.py

    returns string: 'tag'-'commits since tag'-'7 digit commit id'
    """
    try:
        return check_output(['git', 'describe', '--tags'],stderr=STDOUT,cwd=os.path.dirname(os.path.abspath(__file__)))
    except CalledProcessError as e:
        logger.error('Error calling git: "{}" \n output: "{}"'.format(e,e.output))
        return None
    except OSError as e:
        logger.error('Could not call git, is it installed? error msg: "{}"'.format(e))
        return None

def compress_folder(src_dir, dst_filename):
    shutil.make_archive(dst_filename, 'zip', src_dir)

def delete_folder(dir_name):
    shutil.rmtree(dir_name)

def copy_folder(src, dst):
    shutil.copytree(src, dst)

def copy_file(src, dst):
    shutil.copyfile(src,dst)

LANGUAGES_FOLDER = 'languages'
RELEASES_FOLDER = 'releases'
CONFIG_EXAMPLES_FOLDER = os.path.join('docs','config_examples')
APPLICATION_FILENAME = 'stimulus_control'

if __name__ == "__main__":
    root_path = os.path.dirname(os.path.abspath(__file__))
    root_path = os.path.dirname(root_path)
    language_path = os.path.join(root_path, LANGUAGES_FOLDER)
    print(root_path)

    src_language_files = glob(os.path.join(language_path,'*.po'))

    src_win32_nozmq_libraries = [
        os.path.join(root_path, 'dependency/lbass/windows/32/bass.dll')
    ]

    src_win32_libraries = [
        os.path.join(root_path, 'dependency/lbass/windows/32/bass.dll'),
        os.path.join(root_path, 'dependency/libzmq/win10-32/libzmq.dll')
    ]

    src_win64_libraries = [
        os.path.join(root_path, 'dependency/lbass/windows/64/bass.dll'),
        os.path.join(root_path, 'dependency/libzmq/win10-64/libzmq.dll')
    ]

    src_linux64_libraries = [
        os.path.join(root_path, 'dependency/lbass/linux/64/libbass.so')
    ]

    targets = {
        'stimulus_control_win10_32bits_nozmq': src_win32_nozmq_libraries,
        'stimulus_control_win10_32bits': src_win32_libraries,
        'stimulus_control_win10_64bits': src_win64_libraries,
        'stimulus_control_linux_64bits': src_linux64_libraries
    }

    for build_name, libraries in targets.items(): 
        # use build name as root for each release
        releases = os.path.join(root_path, RELEASES_FOLDER)
        destination = os.path.join(releases, build_name) 
        dst_lang_path = os.path.join(destination, LANGUAGES_FOLDER)
        if os.path.exists(destination):
            delete_folder(destination)
        
        os.makedirs(destination)
        os.makedirs(dst_lang_path)
        
        # copy build.exe as app_name.exe
        if 'win' in build_name:
            src = build_name+'.exe'
            dst = os.path.join(destination, APPLICATION_FILENAME+'.exe')

        if 'linux' in build_name:
            src = build_name+'.bin'
            dst = os.path.join(destination, APPLICATION_FILENAME)

        src = os.path.join(root_path, src)
        copy_file(src, dst)

        # copy dlls
        for src_library in libraries:
            dst_library = os.path.join(destination,os.path.basename(src_library))
            copy_file(src_library, dst_library)

        # copy config_examples as Participante_1
        src_examples = os.path.join(root_path, CONFIG_EXAMPLES_FOLDER)
        dst_examples = os.path.join(destination,'Participante_1')
        copy_folder(src_examples, dst_examples)

        # copy language files
        for src_lang in src_language_files:
            _, po_ext = os.path.splitext(src_lang)
            _, lang_ext = os.path.splitext(_)

            if lang_ext:
                ext = lang_ext+po_ext
            else:
                ext = po_ext

            dst_lang = os.path.join(dst_lang_path, APPLICATION_FILENAME+ext)
            copy_file(src_lang, dst_lang)
            # print(src_lang, dst_lang)

        # zip release destination 
        tag = get_tag_commit()
        if tag:
            tag = get_tag_commit()
            tag = tag[:-1]
            dst_filename = os.path.join(releases, build_name+'_'+tag.decode('utf-8'))
        else:
            dst_filename = os.path.join(releases, build_name)
        compress_folder(destination, dst_filename)

        print(build_name+' released.')