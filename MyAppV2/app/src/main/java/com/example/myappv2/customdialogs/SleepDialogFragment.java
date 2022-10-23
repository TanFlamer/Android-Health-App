package com.example.myappv2.customdialogs;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.DialogFragment;

import com.example.myappv2.R;

import java.util.ArrayList;

public class SleepDialogFragment extends DialogFragment {

    String date;

    public SleepDialogFragment(int i, int i1, int i2) {
        date = "" + i + i1 + i2;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        return inflater.inflate(R.layout.fragment_sleep_dialog, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        ListView listView = (ListView) requireView().findViewById(R.id.sleepInfo);
        ArrayList<String> info = new ArrayList<>();
        info.add(date);
        ArrayAdapter arrayAdapter = new ArrayAdapter(getContext(), android.R.layout.simple_list_item_1, info);
        listView.setAdapter(arrayAdapter);

        Button edit = requireView().findViewById(R.id.sleepEdit);
        edit.setOnClickListener(view1 -> {
            Toast.makeText(getActivity(),"test", Toast.LENGTH_SHORT).show();
        });
    }
}
