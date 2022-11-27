package com.example.myapp.fragments.music.musicList;

import android.Manifest;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databaseFiles.song.Song;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link MusicList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class MusicList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public MusicList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment Songs.
     */
    // TODO: Rename and change types and number of parameters
    public static MusicList newInstance(String param1, String param2) {
        MusicList fragment = new MusicList();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    MusicListViewModel musicListViewModel;
    FloatingActionButton floatingActionButton;
    Spinner dataSpinner, orderSpinner;
    ListView listView;

    MusicListAdapter musicListAdapter;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
        musicListViewModel = new ViewModelProvider(this).get(MusicListViewModel.class);
        checkFolderExists();
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseListView();
        initialiseSpinners();
        initialiseFloatingButton();
    }

    public void initialiseListView(){
        listView = requireView().findViewById(R.id.musicListView);
        listView.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        listView.setOnItemClickListener(onItemClickListener);
        listView.setOnItemLongClickListener(onItemLongClickListener);
        listView.setOnItemSelectedListener(onItemSelectedListener);

        musicListAdapter = new MusicListAdapter(requireContext(), 0, new ArrayList<>());
        listView.setAdapter(musicListAdapter);
        musicListViewModel.getSongList().observe(getViewLifecycleOwner(), songList -> {
            Toast.makeText(getContext(), "Dataset changed", Toast.LENGTH_SHORT).show();
            musicListAdapter.updateSongList(songList);
        });
    }

    public AdapterView.OnItemClickListener onItemClickListener = new AdapterView.OnItemClickListener() {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) listView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " clicked", Toast.LENGTH_SHORT).show();
            musicListViewModel.getMusicPlayer().setPlaylist(musicListAdapter.getSongList(), position);
        }
    };

    public AdapterView.OnItemLongClickListener onItemLongClickListener = new AdapterView.OnItemLongClickListener() {
        @Override
        public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) listView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " long clicked", Toast.LENGTH_SHORT).show();
            return true;
        }
    };

    public AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            Song song = (Song) listView.getItemAtPosition(position);
            Toast.makeText(getContext(), song.getSongName() + " selected", Toast.LENGTH_SHORT).show();
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {
            Toast.makeText(getContext(), "Item unselected", Toast.LENGTH_SHORT).show();
        }
    };

    public void initialiseSpinners(){
        dataSpinner = requireView().findViewById(R.id.dataSpinner);
        orderSpinner = requireView().findViewById(R.id.orderSpinner);
    }

    public void initialiseFloatingButton(){
        floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> getMusicFile());
    }

    public void getMusicFile(){
        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            mGetContent.launch("audio/*");
        else
            requestPermissionLauncher.launch(Manifest.permission.READ_EXTERNAL_STORAGE);
    }

    public void checkFolderExists(){
        File newFolder = new File(musicListViewModel.getFilePath());
        if(!newFolder.exists()){
            boolean folderCreation = newFolder.mkdirs();
            Toast.makeText(getContext(), "Folder creation " + (folderCreation ? "successful" : "failed"), Toast.LENGTH_SHORT).show();
        }
    }

    private final ActivityResultLauncher<String> mGetContent = registerForActivityResult(new ActivityResultContracts.GetMultipleContents(),
            uri -> {
                if(uri.size() != 0){
                    for(int i = 0; i < uri.size(); i++){
                        Uri currentUri = uri.get(i);
                        try {
                            InputStream source = requireActivity().getContentResolver().openInputStream(currentUri);
                            String fileName = new File(currentUri.getPath()).getName();
                            Path dest = Paths.get(musicListViewModel.getFilePath(), fileName);
                            Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
                            musicListViewModel.processFile(fileName, currentUri);
                            Toast.makeText(getContext(), fileName + " copied successfully", Toast.LENGTH_SHORT).show();
                        }
                        catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
                else
                    Toast.makeText(getContext(), "No file selected", Toast.LENGTH_SHORT).show();
            });

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    mGetContent.launch("audio/*");
                }
                else {
                    Toast.makeText(getContext(), "Permission not granted to copy files", Toast.LENGTH_SHORT).show();
                }
            });
}