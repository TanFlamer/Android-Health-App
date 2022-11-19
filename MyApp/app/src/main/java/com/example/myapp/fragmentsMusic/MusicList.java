package com.example.myapp.fragmentsMusic;

import android.Manifest;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.viewModal.MusicListViewModel;
import com.example.myapp.fragmentsMusic.listMusic.MusicListAdapter;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

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

        ListView listView = requireView().findViewById(R.id.musicListView);
        List<Song> songList = new ArrayList<>();

        songList.add(new Song("test", Duration.ofMinutes(2), musicListViewModel.getUserID()));
        songList.add(new Song("test1", Duration.ofMinutes(1), musicListViewModel.getUserID()));

        MusicListAdapter musicListAdapter = new MusicListAdapter(getContext(), R.layout.music_list_item, songList);
        listView.setAdapter(musicListAdapter);

        FloatingActionButton floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> getMusicFile());
    }

    public void getMusicFile(){
        if (ContextCompat.checkSelfPermission(getContext(), Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)
            mGetContent.launch("audio/*");
        else
            requestPermissionLauncher.launch(Manifest.permission.WRITE_EXTERNAL_STORAGE);
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
                        String oldFilePath = uri.get(i).getPath().split(":")[1];
                        Path source = Paths.get(oldFilePath);
                        Path dest = Paths.get(musicListViewModel.getFilePath(), new File(oldFilePath).getName());
                        try {
                            Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING);
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
                    Toast.makeText(getContext(), "Permission not granted to move files", Toast.LENGTH_SHORT).show();
                }
            });
}