package com.example.myappv2.subfragments.sleep;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.Toast;

import com.example.myappv2.R;
import com.example.myappv2.recyclers.SleepRecyclerAdapter;
import com.example.myappv2.recyclers.SleepRecyclerItem;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SleepList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SleepList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SleepList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepList.
     */
    // TODO: Rename and change types and number of parameters
    public static SleepList newInstance(String param1, String param2) {
        SleepList fragment = new SleepList();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sleep_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        ListView listView = requireView().findViewById(R.id.sleepListInfo);
        List<SleepListItem> sleepListItemList = new ArrayList<>();
        sleepListItemList.add(new SleepListItem("test", "test","test", 0));
        sleepListItemList.add(new SleepListItem("test1", "test1","test1", 1));
        SleepListAdapter sleepListAdapter = new SleepListAdapter(getContext(), R.layout.sleep_list_item, sleepListItemList);
        listView.setAdapter(sleepListAdapter);

        listView.setOnItemClickListener((adapterView, view1, i, l) -> {
            SleepListItem sleepListItem = (SleepListItem) listView.getItemAtPosition(i);
            Toast.makeText(getContext(), sleepListItem.getDate(), Toast.LENGTH_SHORT).show();
        });

        //for recycler view use
        /*RecyclerView recyclerView = requireView().findViewById(R.id.sleepRecycler);
        List<SleepRecyclerItem> items = new ArrayList<>();
        items.add(new SleepRecyclerItem("test1","test1","test1", 0));
        items.add(new SleepRecyclerItem("test2","test2","test2", 0));
        recyclerView.setAdapter(new SleepRecyclerAdapter(getContext(), items));
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));*/
    }
}